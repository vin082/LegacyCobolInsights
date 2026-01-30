"""
Tech Debt Analyzer Agent - Analyzes and visualizes technical debt in legacy COBOL systems

This agent evaluates technical debt by analyzing:
- Code complexity and maintainability
- Dead code and unused components
- Coupling and dependency issues
- Anti-patterns (GO TO, ALTER, etc.)
- Documentation gaps
- Error handling completeness
"""

import logging
from typing import Dict, List, Any
from datetime import datetime

from utils.neo4j_client import neo4j_client
from utils.state import TypedDict, Annotated, operator

logger = logging.getLogger(__name__)


class TechDebtState(TypedDict):
    """State for technical debt analysis"""

    # Input configuration
    filters: Dict[str, Any]  # Optional filters (domain, max_programs)

    # Processing status
    status: str  # pending, analyzing, completed, failed
    errors: Annotated[List[str], operator.add]

    # Output - debt metrics
    debt_metrics: Dict[str, Any]  # Overall metrics
    program_debts: List[Dict[str, Any]]  # Per-program debt scores

    # Metrics
    analysis_time: float
    timestamp: str


class TechDebtAnalyzerAgent:
    """Agent for analyzing technical debt in legacy COBOL systems"""

    def __init__(self):
        self.neo4j = neo4j_client

    def process(self, state: TechDebtState) -> Dict:
        """
        Main processing method for technical debt analysis

        Args:
            state: TechDebtState with analysis parameters

        Returns:
            Updated state with debt metrics and scores
        """
        logger.info("📊 Starting technical debt analysis...")
        start_time = datetime.now()

        try:
            state['status'] = 'analyzing'

            filters = state.get('filters', {})

            # Gather debt metrics from knowledge graph
            program_debts = self._analyze_program_debt(filters)
            overall_metrics = self._calculate_overall_metrics(program_debts)

            state['debt_metrics'] = overall_metrics
            state['program_debts'] = program_debts
            state['status'] = 'completed'
            state['analysis_time'] = (datetime.now() - start_time).total_seconds()
            state['timestamp'] = datetime.now().isoformat()

            logger.info(f"✅ Tech debt analysis complete in {state['analysis_time']:.2f}s")
            logger.info(f"   Analyzed {len(program_debts)} programs")
            logger.info(f"   Average debt score: {overall_metrics['avg_debt_score']:.1f}/100")

            return state

        except Exception as e:
            logger.error(f"❌ Error in tech debt analysis: {e}", exc_info=True)
            state['status'] = 'failed'
            state['errors'].append(str(e))
            return state

    def _analyze_program_debt(self, filters: Dict) -> List[Dict]:
        """
        Query Neo4j for comprehensive debt indicators per program

        Args:
            filters: Dictionary with optional filters

        Returns:
            List of program debt dictionaries with scores
        """
        # Build WHERE clause
        where_conditions = []

        if 'domain' in filters and filters['domain']:
            where_conditions.append(f"toLower(COALESCE(p.domain, '')) CONTAINS toLower('{filters['domain']}')")

        where_clause = "WHERE " + " AND ".join(where_conditions) if where_conditions else ""
        limit = filters.get('max_programs', 50)

        # Comprehensive debt analysis query
        query = f"""
        MATCH (p:CobolProgram)
        {where_clause}

        OPTIONAL MATCH (p)-[:CALLS]->(called:CobolProgram)
        OPTIONAL MATCH (caller:CobolProgram)-[:CALLS]->(p)
        OPTIONAL MATCH (p)-[:USES]->(cb:Copybook)

        WITH p,
             COUNT(DISTINCT called) AS calls_out,
             COUNT(DISTINCT caller) AS calls_in,
             COUNT(DISTINCT cb) AS copybooks_used

        RETURN p.name AS name,
               COALESCE(p.domain, 'Unknown') AS domain,
               COALESCE(p.complexity_score, 0) AS complexity,
               COALESCE(p.loc, 0) AS loc,
               calls_out,
               calls_in,
               copybooks_used,
               COALESCE(p.description, '') AS description,
               COALESCE(p.business_logic, '') AS business_logic,
               p.code AS source_code
        ORDER BY COALESCE(p.complexity_score, 0) DESC
        LIMIT {limit}
        """

        try:
            programs = self.neo4j.query(query)
            logger.info(f"Retrieved {len(programs)} programs for debt analysis")

            # Calculate debt scores for each program
            program_debts = []
            for prog in programs:
                debt_score = self._calculate_debt_score(prog)
                program_debts.append(debt_score)

            # Sort by total debt score (descending)
            program_debts.sort(key=lambda x: x['total_debt_score'], reverse=True)

            return program_debts

        except Exception as e:
            logger.error(f"Error querying Neo4j for debt analysis: {e}", exc_info=True)
            return []

    def _calculate_debt_score(self, program: Dict) -> Dict:
        """
        Calculate comprehensive debt score for a program

        Debt components (each 0-100):
        1. Complexity debt (30%): High cyclomatic complexity
        2. Coupling debt (25%): High dependencies
        3. Size debt (20%): Large, monolithic code
        4. Documentation debt (15%): Missing descriptions
        5. Code quality debt (10%): Anti-patterns from source analysis

        Args:
            program: Program data from Neo4j

        Returns:
            Dictionary with detailed debt breakdown
        """
        name = program['name']

        # 1. Complexity debt (0-100)
        complexity = program['complexity'] or 0
        complexity_debt = min(complexity, 100)

        # 2. Coupling debt (0-100)
        calls_in = program['calls_in'] or 0
        calls_out = program['calls_out'] or 0
        total_coupling = calls_in + calls_out
        coupling_debt = min((total_coupling / 20) * 100, 100)  # 20+ connections = max debt

        # 3. Size debt (0-100)
        loc = program['loc'] or 0
        size_debt = min((loc / 2000) * 100, 100)  # 2000+ LOC = max debt

        # 4. Documentation debt (0-100)
        description = program['description'] or ''
        business_logic = program['business_logic'] or ''

        doc_debt = 100  # Start with max debt
        if len(description) > 50:
            doc_debt -= 30
        if len(business_logic) > 100:
            doc_debt -= 40
        if len(business_logic) > 300:
            doc_debt -= 30
        doc_debt = max(doc_debt, 0)

        # 5. Code quality debt (0-100) - analyze source for anti-patterns
        code_quality_debt = self._analyze_code_quality(program['source_code'])

        # Calculate weighted total debt score
        total_debt_score = (
            complexity_debt * 0.30 +
            coupling_debt * 0.25 +
            size_debt * 0.20 +
            doc_debt * 0.15 +
            code_quality_debt * 0.10
        )

        # Determine severity level
        if total_debt_score >= 70:
            severity = "Critical"
        elif total_debt_score >= 50:
            severity = "High"
        elif total_debt_score >= 30:
            severity = "Medium"
        else:
            severity = "Low"

        # Identify top debt contributors
        debt_components = {
            'complexity': complexity_debt,
            'coupling': coupling_debt,
            'size': size_debt,
            'documentation': doc_debt,
            'code_quality': code_quality_debt
        }
        top_issues = sorted(debt_components.items(), key=lambda x: x[1], reverse=True)[:3]

        return {
            'program_name': name,
            'domain': program['domain'],
            'total_debt_score': round(total_debt_score, 1),
            'severity': severity,
            'complexity_debt': round(complexity_debt, 1),
            'coupling_debt': round(coupling_debt, 1),
            'size_debt': round(size_debt, 1),
            'documentation_debt': round(doc_debt, 1),
            'code_quality_debt': round(code_quality_debt, 1),
            'top_issues': [issue[0] for issue in top_issues],
            'loc': loc,
            'calls_in': calls_in,
            'calls_out': calls_out,
            'recommendations': self._generate_recommendations(
                name, severity, top_issues, loc, calls_in
            )
        }

    def _analyze_code_quality(self, source_code: str) -> float:
        """
        Analyze source code for anti-patterns and quality issues

        Args:
            source_code: COBOL source code

        Returns:
            Code quality debt score (0-100)
        """
        if not source_code:
            return 50  # Unknown, assume moderate debt

        code_upper = source_code.upper()
        code_lines = source_code.split('\n')

        debt_score = 0

        # Count anti-patterns
        goto_count = code_upper.count('GO TO')
        alter_count = code_upper.count('ALTER')
        stop_run_count = code_upper.count('STOP RUN')

        # Penalize anti-patterns
        debt_score += min(goto_count * 5, 40)  # Max 40 points for GOTO
        debt_score += min(alter_count * 10, 30)  # Max 30 points for ALTER (worse)
        debt_score += min(stop_run_count * 3, 15)  # Max 15 points for STOP RUN

        # Check for error handling
        has_error_handling = any(
            keyword in code_upper
            for keyword in ['ON ERROR', 'INVALID KEY', 'AT END', 'ON EXCEPTION']
        )

        if not has_error_handling and len(code_lines) > 100:
            debt_score += 15  # No error handling in large program

        return min(debt_score, 100)

    def _generate_recommendations(self, name: str, severity: str,
                                  top_issues: List, loc: int, calls_in: int) -> List[str]:
        """
        Generate actionable recommendations based on debt analysis

        Args:
            name: Program name
            severity: Debt severity level
            top_issues: Top 3 debt contributors
            loc: Lines of code
            calls_in: Number of callers

        Returns:
            List of recommendation strings
        """
        recommendations = []

        # Severity-based recommendations
        if severity == "Critical":
            recommendations.append("⚠️ URGENT: This program requires immediate attention")
            recommendations.append("Consider refactoring or rewriting this component")
        elif severity == "High":
            recommendations.append("High priority for modernization planning")

        # Component-specific recommendations
        for issue_name, score in top_issues:
            if issue_name == 'complexity' and score >= 60:
                recommendations.append("Break down complex logic into smaller, testable functions")

            if issue_name == 'coupling' and score >= 60:
                if calls_in > 10:
                    recommendations.append(f"High coupling risk: {calls_in} programs depend on this")
                recommendations.append("Consider API boundaries to reduce coupling")

            if issue_name == 'size' and score >= 60:
                recommendations.append(f"Large codebase ({loc} LOC): Split into smaller modules")

            if issue_name == 'documentation' and score >= 70:
                recommendations.append("Add comprehensive documentation and business logic notes")

            if issue_name == 'code_quality' and score >= 50:
                recommendations.append("Refactor anti-patterns (GO TO, ALTER statements)")

        if not recommendations:
            recommendations.append("Maintain current quality standards")

        return recommendations[:5]  # Limit to top 5 recommendations

    def _calculate_overall_metrics(self, program_debts: List[Dict]) -> Dict:
        """
        Calculate overall system-wide debt metrics

        Args:
            program_debts: List of program debt dictionaries

        Returns:
            Dictionary with aggregate metrics
        """
        if not program_debts:
            return {
                'total_programs': 0,
                'avg_debt_score': 0,
                'critical_count': 0,
                'high_count': 0,
                'medium_count': 0,
                'low_count': 0,
                'total_loc': 0,
                'highest_debt_programs': []
            }

        total_debt = sum(p['total_debt_score'] for p in program_debts)
        avg_debt = total_debt / len(program_debts)

        severity_counts = {
            'Critical': len([p for p in program_debts if p['severity'] == 'Critical']),
            'High': len([p for p in program_debts if p['severity'] == 'High']),
            'Medium': len([p for p in program_debts if p['severity'] == 'Medium']),
            'Low': len([p for p in program_debts if p['severity'] == 'Low'])
        }

        total_loc = sum(p['loc'] for p in program_debts)

        # Top 10 highest debt programs
        highest_debt = program_debts[:10]

        return {
            'total_programs': len(program_debts),
            'avg_debt_score': round(avg_debt, 1),
            'critical_count': severity_counts['Critical'],
            'high_count': severity_counts['High'],
            'medium_count': severity_counts['Medium'],
            'low_count': severity_counts['Low'],
            'total_loc': total_loc,
            'highest_debt_programs': [
                {
                    'name': p['program_name'],
                    'score': p['total_debt_score'],
                    'severity': p['severity']
                }
                for p in highest_debt
            ]
        }


# Create singleton instance
tech_debt_analyzer = TechDebtAnalyzerAgent()
