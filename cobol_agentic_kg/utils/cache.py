"""
Caching utility with Redis support and in-memory fallback
"""
import json
import hashlib
from typing import Optional, Any, Dict
from datetime import timedelta
from utils.logger import logger


class CacheClient:
    """
    Caching client with Redis backend and in-memory fallback
    """

    def __init__(self, use_redis: bool = True, redis_url: Optional[str] = None, default_ttl: int = 3600):
        """
        Initialize cache client

        Args:
            use_redis: Whether to use Redis (fallback to in-memory if False or connection fails)
            redis_url: Redis connection URL (default: redis://localhost:6379/0)
            default_ttl: Default time-to-live in seconds (default: 1 hour)
        """
        self.default_ttl = default_ttl
        self.redis_client = None
        self.memory_cache: Dict[str, Any] = {}
        self.use_redis = use_redis

        if use_redis:
            try:
                import redis
                url = redis_url or "redis://localhost:6379/0"
                self.redis_client = redis.from_url(
                    url,
                    decode_responses=True,
                    socket_connect_timeout=2,
                    socket_timeout=2
                )
                # Test connection
                self.redis_client.ping()
                logger.info("✅ Redis cache connected")
            except Exception as e:
                logger.warning(f"⚠️ Redis unavailable, using in-memory cache: {e}")
                self.redis_client = None
        else:
            logger.info("📦 Using in-memory cache (Redis disabled)")

    def _generate_key(self, prefix: str, data: str) -> str:
        """
        Generate cache key from prefix and data

        Args:
            prefix: Key prefix (e.g., 'query', 'cypher')
            data: Data to hash

        Returns:
            Cache key
        """
        hash_value = hashlib.md5(data.encode()).hexdigest()
        return f"{prefix}:{hash_value}"

    def get(self, key: str) -> Optional[Any]:
        """
        Get value from cache

        Args:
            key: Cache key

        Returns:
            Cached value or None if not found
        """
        try:
            if self.redis_client:
                value = self.redis_client.get(key)
                if value:
                    logger.debug(f"🎯 Cache HIT (Redis): {key}")
                    return json.loads(value)
            else:
                if key in self.memory_cache:
                    logger.debug(f"🎯 Cache HIT (Memory): {key}")
                    return self.memory_cache[key]

            logger.debug(f"❌ Cache MISS: {key}")
            return None

        except Exception as e:
            logger.error(f"Cache get error: {e}")
            return None

    def set(self, key: str, value: Any, ttl: Optional[int] = None) -> bool:
        """
        Set value in cache

        Args:
            key: Cache key
            value: Value to cache
            ttl: Time-to-live in seconds (None = use default)

        Returns:
            True if successful
        """
        try:
            ttl = ttl or self.default_ttl

            if self.redis_client:
                serialized = json.dumps(value, default=str)
                self.redis_client.setex(key, ttl, serialized)
                logger.debug(f"💾 Cache SET (Redis): {key} (TTL: {ttl}s)")
            else:
                self.memory_cache[key] = value
                logger.debug(f"💾 Cache SET (Memory): {key}")

            return True

        except Exception as e:
            logger.error(f"Cache set error: {e}")
            return False

    def delete(self, key: str) -> bool:
        """
        Delete key from cache

        Args:
            key: Cache key

        Returns:
            True if successful
        """
        try:
            if self.redis_client:
                self.redis_client.delete(key)
            else:
                self.memory_cache.pop(key, None)

            logger.debug(f"🗑️ Cache DELETE: {key}")
            return True

        except Exception as e:
            logger.error(f"Cache delete error: {e}")
            return False

    def clear(self, pattern: Optional[str] = None) -> int:
        """
        Clear cache entries

        Args:
            pattern: Key pattern to match (e.g., 'query:*'), None = clear all

        Returns:
            Number of keys deleted
        """
        try:
            if self.redis_client:
                if pattern:
                    keys = self.redis_client.keys(pattern)
                    if keys:
                        deleted = self.redis_client.delete(*keys)
                        logger.info(f"🗑️ Cleared {deleted} cache entries matching '{pattern}'")
                        return deleted
                else:
                    self.redis_client.flushdb()
                    logger.info("🗑️ Cleared all cache entries")
                    return -1  # Unknown count
            else:
                if pattern:
                    # Simple pattern matching for memory cache
                    pattern_prefix = pattern.replace('*', '')
                    to_delete = [k for k in self.memory_cache.keys() if k.startswith(pattern_prefix)]
                    for k in to_delete:
                        del self.memory_cache[k]
                    logger.info(f"🗑️ Cleared {len(to_delete)} cache entries matching '{pattern}'")
                    return len(to_delete)
                else:
                    count = len(self.memory_cache)
                    self.memory_cache.clear()
                    logger.info(f"🗑️ Cleared {count} cache entries")
                    return count

            return 0

        except Exception as e:
            logger.error(f"Cache clear error: {e}")
            return 0

    def get_stats(self) -> Dict[str, Any]:
        """
        Get cache statistics

        Returns:
            Dictionary with cache stats
        """
        try:
            if self.redis_client:
                info = self.redis_client.info()
                return {
                    "backend": "redis",
                    "connected": True,
                    "keys": self.redis_client.dbsize(),
                    "memory_used": info.get("used_memory_human", "N/A"),
                    "hits": info.get("keyspace_hits", 0),
                    "misses": info.get("keyspace_misses", 0)
                }
            else:
                return {
                    "backend": "memory",
                    "connected": True,
                    "keys": len(self.memory_cache),
                    "memory_used": "N/A",
                    "hits": "N/A",
                    "misses": "N/A"
                }

        except Exception as e:
            logger.error(f"Cache stats error: {e}")
            return {
                "backend": "redis" if self.redis_client else "memory",
                "connected": False,
                "error": str(e)
            }


# Singleton cache instance
cache_client: Optional[CacheClient] = None


def get_cache() -> CacheClient:
    """Get or create cache client singleton"""
    global cache_client

    if cache_client is None:
        from config.settings import settings

        cache_client = CacheClient(
            use_redis=settings.cache_enabled and settings.redis_enabled,
            redis_url=settings.redis_url,
            default_ttl=settings.cache_ttl
        )

    return cache_client
