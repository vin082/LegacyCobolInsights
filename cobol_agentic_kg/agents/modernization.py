"""
Modernization Agent - Analyzes COBOL programs and provides modernization recommendations

This agent evaluates programs for:
- Technical debt and modernization urgency
- Business value and strategic importance
- Migration risk assessment
- Recommended modernization strategies
- Technology recommendations
"""

import logging
from typing import Dict, List, Tuple
from datetime import datetime

from utils.neo4j_client import neo4j_client
from utils.llm_factory import get_llm
from utils.state import ModernizationState

logger = logging.getLogger(__name__)


class ModernizationAgent:
    """Agent for analyzing programs and providing modernization recommendations"""

    def __init__(self):
        self.neo4j = neo4j_client
        self._llm = None

    @property
    def llm(self):
        """Lazy initialization of LLM"""
        if self._llm is None:
            self._llm = get_llm(temperature=0.2, max_tokens=1000)
        return self._llm

    def process(self, state: ModernizationState) -> Dict:
        """
        Main processing method for modernization analysis

        Args:
            state: ModernizationState with analysis parameters

        Returns:
            Updated state with recommendations
        """
        logger.info("🔧 Starting modernization analysis...")
        start_time = datetime.now()

        try:
            state['status'] = 'analyzing'

            # Gather program data from knowledge graph
            filters = state.get('filters', {})
            programs = self._gather_program_data(filters)

            if not programs:
                logger.warning("No programs found for modernization analysis")
                state['status'] = 'completed'
                state['recommendations'] = []
                state['analysis_time'] = 0.0
                return state

            logger.info(f"Analyzing {len(programs)} programs for modernization...")

            # Analyze each program
            recommendations = []
            for i, program in enumerate(programs, 1):
                logger.info(f"  [{i}/{len(programs)}] Analyzing {program['name']}...")

                recommendation = self._analyze_program(program)
                recommendations.append(recommendation)

            # Sort by priority score (descending)
            recommendations.sort(key=lambda x: x['priority_score'], reverse=True)

            state['recommendations'] = recommendations
            state['status'] = 'completed'
            state['analysis_time'] = (datetime.now() - start_time).total_seconds()
            state['timestamp'] = datetime.now().isoformat()

            logger.info(f"✅ Modernization analysis complete in {state['analysis_time']:.2f}s")
            return state

        except Exception as e:
            logger.error(f"❌ Error in modernization analysis: {e}", exc_info=True)
            state['status'] = 'failed'
            state['errors'].append(str(e))
            return state

    def _gather_program_data(self, filters: Dict) -> List[Dict]:
        """
        Query Neo4j for comprehensive program data

        Args:
            filters: Dictionary with optional filters (max_programs, complexity, domain)

        Returns:
            List of program data dictionaries
        """
        # Build WHERE clause from filters
        where_conditions = []

        if 'domain' in filters and filters['domain']:
            where_conditions.append(f"toLower(COALESCE(p.domain, '')) CONTAINS toLower('{filters['domain']}')")

        if 'complexity' in filters and filters['complexity']:
            complexity = filters['complexity'].lower()
            if complexity == 'high':
                where_conditions.append("COALESCE(p.complexity_score, 0) >= 70")
            elif complexity == 'medium':
                where_conditions.append("COALESCE(p.complexity_score, 0) >= 40 AND COALESCE(p.complexity_score, 0) < 70")
            elif complexity == 'low':
                where_conditions.append("COALESCE(p.complexity_score, 0) < 40")

        where_clause = "WHERE " + " AND ".join(where_conditions) if where_conditions else ""

        # Get program limit
        limit = filters.get('max_programs', 20)

        # Query to get comprehensive program data
        query = f"""
        MATCH (p:CobolProgram)
        {where_clause}

        OPTIONAL MATCH (p)-[:CALLS]->(called:CobolProgram)
        OPTIONAL MATCH (caller:CobolProgram)-[:CALLS]->(p)
        OPTIONAL MATCH (p)-[:READS]->(reads:DataFile)
        OPTIONAL MATCH (p)-[:WRITES]->(writes:DataFile)

        WITH p,
             COUNT(DISTINCT called) AS calls_out,
             COUNT(DISTINCT caller) AS calls_in,
             COUNT(DISTINCT reads) AS files_read,
             COUNT(DISTINCT writes) AS files_written,
             SIZE(COALESCE(p.code, '')) AS code_length

        RETURN p.name AS name,
               COALESCE(p.domain, 'Unknown') AS domain,
               p.description AS description,
               COALESCE(p.business_logic, '') AS business_logic,
               COALESCE(p.complexity_score, 0) AS complexity,
               COALESCE(p.loc, CASE WHEN code_length > 0 THEN code_length / 80 ELSE 0 END) AS loc,
               calls_out,
               calls_in,
               files_read,
               files_written,
               p.code AS source_code
        ORDER BY COALESCE(p.complexity_score, calls_in + calls_out) DESC
        LIMIT {limit}
        """

        try:
            result = self.neo4j.execute_query(query)
            programs = [dict(record) for record in result]
            logger.info(f"Retrieved {len(programs)} programs for modernization analysis")
            return programs
        except Exception as e:
            logger.error(f"Error querying Neo4j: {e}")
            return []

    def _analyze_program(self, program: Dict) -> Dict:
        """
        Analyze a single program and generate modernization recommendation

        Args:
            program: Program data dictionary

        Returns:
            Recommendation dictionary with scores and strategies
        """
        name = program['name']

        # Calculate risk score (0-100)
        risk_score = self._calculate_risk_score(program)

        # Calculate business value score (0-100)
        value_score = self._calculate_value_score(program)

        # Calculate priority score (weighted combination)
        priority_score = (risk_score * 0.4) + (value_score * 0.6)

        # Determine modernization strategy
        strategy = self._determine_strategy(risk_score, value_score, program)

        # Get LLM-powered recommendations
        llm_recommendation = self._get_llm_recommendation(program, risk_score, value_score, strategy)

        recommendation = {
            'program_name': name,
            'domain': program['domain'],
            'complexity': program['complexity'] or 0,
            'loc': program['loc'] or 0,
            'risk_score': round(risk_score, 1),
            'value_score': round(value_score, 1),
            'priority_score': round(priority_score, 1),
            'strategy': strategy,
            'risk_factors': self._identify_risk_factors(program),
            'value_factors': self._identify_value_factors(program),
            'recommended_approach': llm_recommendation['approach'],
            'technology_recommendations': llm_recommendation['technologies'],
            'estimated_effort': llm_recommendation['effort'],
            'key_considerations': llm_recommendation['considerations']
        }

        return recommendation

    def _calculate_risk_score(self, program: Dict) -> float:
        """
        Calculate modernization risk score (0-100, higher = riskier)

        Factors:
        - High complexity (40%)
        - High coupling (30%)
        - Large size (20%)
        - Heavy file I/O (10%)
        """
        score = 0.0

        # Complexity (0-40 points)
        complexity = program['complexity'] or 0
        score += min(complexity * 0.4, 40)

        # Coupling - high calls_in means many dependencies (0-30 points)
        calls_in = program['calls_in'] or 0
        coupling_score = min((calls_in / 10) * 30, 30)  # Normalize: 10+ callers = max
        score += coupling_score

        # Size (0-20 points)
        loc = program['loc'] or 0
        size_score = min((loc / 1000) * 20, 20)  # Normalize: 1000+ LOC = max
        score += size_score

        # File I/O complexity (0-10 points)
        files_total = (program['files_read'] or 0) + (program['files_written'] or 0)
        io_score = min((files_total / 5) * 10, 10)  # Normalize: 5+ files = max
        score += io_score

        return min(score, 100)

    def _calculate_value_score(self, program: Dict) -> float:
        """
        Calculate business value score (0-100, higher = more valuable)

        Factors:
        - High usage (calls_in) (40%)
        - Critical domain (30%)
        - Complex business logic (20%)
        - Integration hub (calls_out) (10%)
        """
        score = 0.0

        # Usage - high calls_in means widely used (0-40 points)
        calls_in = program['calls_in'] or 0
        usage_score = min((calls_in / 10) * 40, 40)  # 10+ callers = max value
        score += usage_score

        # Domain criticality (0-30 points)
        domain = (program['domain'] or '').lower()
        critical_domains = ['billing', 'payment', 'financial', 'customer', 'transaction', 'core']
        if any(d in domain for d in critical_domains):
            score += 30
        elif domain != 'unknown':
            score += 15

        # Business logic complexity (0-20 points)
        business_logic = program['business_logic'] or ''
        if len(business_logic) > 200:
            score += 20
        elif len(business_logic) > 50:
            score += 10

        # Integration hub - calls many programs (0-10 points)
        calls_out = program['calls_out'] or 0
        integration_score = min((calls_out / 5) * 10, 10)  # 5+ calls = max
        score += integration_score

        return min(score, 100)

    def _determine_strategy(self, risk_score: float, value_score: float, program: Dict) -> str:
        """
        Determine recommended modernization strategy based on risk/value quadrants

        Quadrants:
        - High Value, Low Risk -> Rewrite (ideal candidate)
        - High Value, High Risk -> Strangler Fig (gradual migration)
        - Low Value, Low Risk -> Retire/Replace (if possible)
        - Low Value, High Risk -> Encapsulate (wrap and minimize changes)
        """
        if value_score >= 60 and risk_score < 50:
            return "Rewrite"
        elif value_score >= 60 and risk_score >= 50:
            return "Strangler Fig Pattern"
        elif value_score < 60 and risk_score < 50:
            return "Retire/Replace"
        else:  # Low value, high risk
            return "Encapsulate & Modernize"

    def _identify_risk_factors(self, program: Dict) -> List[str]:
        """Identify specific risk factors for a program"""
        factors = []

        complexity = program['complexity'] or 0
        if complexity >= 70:
            factors.append(f"Very high complexity ({complexity})")
        elif complexity >= 40:
            factors.append(f"Moderate complexity ({complexity})")

        calls_in = program['calls_in'] or 0
        if calls_in >= 10:
            factors.append(f"High coupling ({calls_in} programs depend on this)")
        elif calls_in >= 5:
            factors.append(f"Moderate coupling ({calls_in} programs depend on this)")

        loc = program['loc'] or 0
        if loc >= 1000:
            factors.append(f"Large codebase ({int(loc)} LOC)")
        elif loc >= 500:
            factors.append(f"Medium-sized codebase ({int(loc)} LOC)")

        files_total = (program['files_read'] or 0) + (program['files_written'] or 0)
        if files_total >= 5:
            factors.append(f"Heavy data I/O ({files_total} files)")

        if not factors:
            factors.append("Relatively low risk profile")

        return factors

    def _identify_value_factors(self, program: Dict) -> List[str]:
        """Identify specific value factors for a program"""
        factors = []

        calls_in = program['calls_in'] or 0
        if calls_in >= 10:
            factors.append(f"Widely used ({calls_in} callers)")
        elif calls_in >= 5:
            factors.append(f"Frequently used ({calls_in} callers)")

        domain = (program['domain'] or '').lower()
        critical_domains = ['billing', 'payment', 'financial', 'customer', 'transaction', 'core']
        if any(d in domain for d in critical_domains):
            factors.append(f"Critical business domain ({program['domain']})")
        elif domain != 'unknown':
            factors.append(f"Business domain: {program['domain']}")

        business_logic = program['business_logic'] or ''
        if len(business_logic) > 200:
            factors.append("Complex business logic")

        calls_out = program['calls_out'] or 0
        if calls_out >= 5:
            factors.append(f"Integration hub ({calls_out} dependencies)")

        if not factors:
            factors.append("Limited identified value factors")

        return factors

    def _get_llm_recommendation(self, program: Dict, risk_score: float,
                                value_score: float, strategy: str) -> Dict:
        """
        Use LLM to generate detailed modernization recommendations

        Args:
            program: Program data
            risk_score: Calculated risk score
            value_score: Calculated value score
            strategy: Recommended strategy

        Returns:
            Dictionary with approach, technologies, effort, considerations
        """
        # Prepare context for LLM
        context = f"""Program: {program['name']}
Domain: {program['domain']}
Complexity: {program['complexity'] or 0}
LOC: {program['loc'] or 0}
Dependencies: {program['calls_in']} programs call this, calls {program['calls_out']} programs
Data I/O: Reads {program['files_read']} files, writes {program['files_written']} files

Risk Score: {risk_score:.1f}/100
Value Score: {value_score:.1f}/100
Recommended Strategy: {strategy}

Business Logic:
{program['business_logic'] or 'Not documented'}
"""

        prompt = f"""You are a legacy modernization consultant analyzing a COBOL program for modernization.

{context}

Provide a detailed modernization recommendation with:

1. **Approach** (2-3 sentences): Explain HOW to implement the {strategy} strategy for this program
2. **Technologies** (bullet list): Recommend specific modern technologies (languages, frameworks, platforms)
3. **Effort Estimate**: Realistic timeframe (e.g., "2-3 months with 2 developers")
4. **Key Considerations** (bullet list): Critical factors to ensure successful migration

Format your response as:

APPROACH:
[2-3 sentences explaining the approach]

TECHNOLOGIES:
- [Technology 1]: [Why it's suitable]
- [Technology 2]: [Why it's suitable]
- [Technology 3]: [Why it's suitable]

EFFORT:
[Timeframe estimate]

CONSIDERATIONS:
- [Consideration 1]
- [Consideration 2]
- [Consideration 3]
"""

        try:
            response = self.llm.invoke(prompt)
            content = response.content

            # Parse LLM response
            recommendation = {
                'approach': self._extract_section(content, 'APPROACH:', 'TECHNOLOGIES:'),
                'technologies': self._extract_list_section(content, 'TECHNOLOGIES:', 'EFFORT:'),
                'effort': self._extract_section(content, 'EFFORT:', 'CONSIDERATIONS:'),
                'considerations': self._extract_list_section(content, 'CONSIDERATIONS:', None)
            }

            return recommendation

        except Exception as e:
            logger.error(f"Error getting LLM recommendation: {e}")
            # Fallback to rule-based recommendations
            return self._get_fallback_recommendation(strategy, program)

    def _extract_section(self, content: str, start_marker: str, end_marker: str) -> str:
        """Extract a text section between markers"""
        try:
            start_idx = content.index(start_marker) + len(start_marker)
            if end_marker:
                end_idx = content.index(end_marker, start_idx)
                section = content[start_idx:end_idx]
            else:
                section = content[start_idx:]
            return section.strip()
        except ValueError:
            return "Not available"

    def _extract_list_section(self, content: str, start_marker: str, end_marker: str) -> List[str]:
        """Extract a bullet list section between markers"""
        try:
            section = self._extract_section(content, start_marker, end_marker)
            lines = section.split('\n')
            items = [line.strip('- ').strip() for line in lines if line.strip().startswith('-')]
            return items if items else ["Details not available"]
        except Exception:
            return ["Details not available"]

    def _get_fallback_recommendation(self, strategy: str, program: Dict) -> Dict:
        """Generate rule-based recommendations when LLM fails"""
        recommendations = {
            "Rewrite": {
                'approach': "Complete rewrite in modern language with microservices architecture. Focus on business logic preservation while improving maintainability.",
                'technologies': [
                    "Java/Spring Boot: Enterprise-grade, widely supported",
                    "PostgreSQL: Modern relational database",
                    "REST APIs: For integration with other services"
                ],
                'effort': "3-6 months with dedicated team",
                'considerations': [
                    "Comprehensive test coverage required",
                    "Parallel run period for validation",
                    "Data migration strategy needed"
                ]
            },
            "Strangler Fig Pattern": {
                'approach': "Gradually replace functionality by building new services around the legacy system. Start with least risky components and incrementally migrate.",
                'technologies': [
                    "Python/FastAPI: Quick development for new services",
                    "API Gateway: Route between old and new systems",
                    "Event-driven architecture: Decouple components"
                ],
                'effort': "6-12 months phased approach",
                'considerations': [
                    "Maintain both systems during transition",
                    "Clear rollback strategy per phase",
                    "Extensive integration testing"
                ]
            },
            "Retire/Replace": {
                'approach': "Replace with commercial off-the-shelf (COTS) solution or modern alternative. Evaluate build vs buy decision.",
                'technologies': [
                    "Low-code platforms: Rapid development",
                    "Cloud SaaS: Reduce maintenance burden",
                    "Open source alternatives: Cost-effective options"
                ],
                'effort': "1-3 months evaluation and migration",
                'considerations': [
                    "Data export and archival required",
                    "Dependency analysis critical",
                    "User training for new system"
                ]
            },
            "Encapsulate & Modernize": {
                'approach': "Wrap legacy code with modern API layer. Minimize changes to core logic while improving accessibility and integration.",
                'technologies': [
                    "REST/GraphQL APIs: Modern integration layer",
                    "Docker containers: Isolate legacy runtime",
                    "API management: Control and monitor access"
                ],
                'effort': "1-2 months for API layer",
                'considerations': [
                    "Legacy system remains largely unchanged",
                    "Focus on interface improvements",
                    "Monitor for future rewrite opportunities"
                ]
            }
        }

        return recommendations.get(strategy, recommendations["Encapsulate & Modernize"])
