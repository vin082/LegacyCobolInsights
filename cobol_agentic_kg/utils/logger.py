"""
Centralized logging configuration
"""
import logging
import sys
from config.settings import settings


def setup_logger(name: str = "cobol_kg") -> logging.Logger:
    """
    Setup and configure logger

    Args:
        name: Logger name

    Returns:
        Configured logger instance
    """
    logger = logging.getLogger(name)

    # Set level from settings
    level = getattr(logging, settings.log_level.upper(), logging.INFO)
    logger.setLevel(level)

    # Create console handler if not already added
    if not logger.handlers:
        console_handler = logging.StreamHandler(sys.stdout)
        console_handler.setLevel(level)

        # Create formatter
        formatter = logging.Formatter(
            '%(asctime)s | %(name)s | %(levelname)s | %(message)s',
            datefmt='%Y-%m-%d %H:%M:%S'
        )
        console_handler.setFormatter(formatter)

        logger.addHandler(console_handler)

    return logger


# Global logger instance
logger = setup_logger()
