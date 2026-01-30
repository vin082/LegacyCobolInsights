"""
Tech Debt Visualizer - Creates interactive visualizations for technical debt analysis

Provides:
- Heatmaps showing debt severity across programs
- Pie charts for debt distribution
- Bar charts for debt component breakdown
- Scatter plots for risk/value analysis
"""

import plotly.graph_objects as go
import plotly.express as px
from typing import Dict, List, Any
import pandas as pd


class TechDebtVisualizer:
    """Creates visualizations for technical debt analysis"""

    # Color scheme for severity levels
    SEVERITY_COLORS = {
        'Critical': '#dc3545',  # Red
        'High': '#fd7e14',      # Orange
        'Medium': '#ffc107',    # Yellow
        'Low': '#28a745'        # Green
    }

    def create_severity_distribution_chart(self, debt_metrics: Dict) -> go.Figure:
        """
        Create pie chart showing distribution of programs by severity

        Args:
            debt_metrics: Overall debt metrics dictionary

        Returns:
            Plotly Figure object
        """
        labels = ['Critical', 'High', 'Medium', 'Low']
        values = [
            debt_metrics['critical_count'],
            debt_metrics['high_count'],
            debt_metrics['medium_count'],
            debt_metrics['low_count']
        ]
        colors = [self.SEVERITY_COLORS[label] for label in labels]

        fig = go.Figure(data=[go.Pie(
            labels=labels,
            values=values,
            marker=dict(colors=colors),
            hole=0.3,
            textinfo='label+value+percent',
            textposition='auto'
        )])

        fig.update_layout(
            title="Technical Debt by Severity Level",
            showlegend=True,
            height=400
        )

        return fig

    def create_top_debt_programs_chart(self, program_debts: List[Dict], top_n: int = 15) -> go.Figure:
        """
        Create horizontal bar chart showing top programs by debt score

        Args:
            program_debts: List of program debt dictionaries
            top_n: Number of top programs to show

        Returns:
            Plotly Figure object
        """
        # Get top N programs
        top_programs = program_debts[:top_n]

        # Prepare data
        program_names = [p['program_name'] for p in top_programs]
        debt_scores = [p['total_debt_score'] for p in top_programs]
        colors = [self.SEVERITY_COLORS[p['severity']] for p in top_programs]

        fig = go.Figure(data=[go.Bar(
            y=program_names,
            x=debt_scores,
            orientation='h',
            marker=dict(color=colors),
            text=debt_scores,
            texttemplate='%{text:.1f}',
            textposition='outside',
            hovertemplate='<b>%{y}</b><br>Debt Score: %{x:.1f}<extra></extra>'
        )])

        fig.update_layout(
            title=f"Top {top_n} Programs by Technical Debt",
            xaxis_title="Debt Score (0-100)",
            yaxis_title="",
            height=max(400, top_n * 30),
            yaxis={'categoryorder': 'total ascending'},
            showlegend=False
        )

        return fig

    def create_debt_components_chart(self, program_debts: List[Dict], top_n: int = 10) -> go.Figure:
        """
        Create stacked bar chart showing debt component breakdown

        Args:
            program_debts: List of program debt dictionaries
            top_n: Number of programs to show

        Returns:
            Plotly Figure object
        """
        # Get top N programs
        top_programs = program_debts[:top_n]

        program_names = [p['program_name'] for p in top_programs]

        # Create traces for each debt component
        fig = go.Figure()

        components = [
            ('Complexity', 'complexity_debt', '#e74c3c'),
            ('Coupling', 'coupling_debt', '#3498db'),
            ('Size', 'size_debt', '#f39c12'),
            ('Documentation', 'documentation_debt', '#9b59b6'),
            ('Code Quality', 'code_quality_debt', '#1abc9c')
        ]

        for comp_name, comp_key, color in components:
            values = [p[comp_key] for p in top_programs]
            fig.add_trace(go.Bar(
                name=comp_name,
                y=program_names,
                x=values,
                orientation='h',
                marker=dict(color=color),
                hovertemplate=f'<b>{comp_name}</b><br>Score: %{{x:.1f}}<extra></extra>'
            ))

        fig.update_layout(
            title=f"Debt Component Breakdown (Top {top_n} Programs)",
            xaxis_title="Debt Score",
            yaxis_title="",
            barmode='stack',
            height=max(400, top_n * 40),
            yaxis={'categoryorder': 'total ascending'},
            legend=dict(
                orientation="h",
                yanchor="bottom",
                y=1.02,
                xanchor="right",
                x=1
            )
        )

        return fig

    def create_debt_heatmap(self, program_debts: List[Dict], max_programs: int = 30) -> go.Figure:
        """
        Create heatmap showing all debt components for programs

        Args:
            program_debts: List of program debt dictionaries
            max_programs: Maximum number of programs to display

        Returns:
            Plotly Figure object
        """
        # Limit to max_programs
        programs = program_debts[:max_programs]

        program_names = [p['program_name'] for p in programs]
        components = ['Complexity', 'Coupling', 'Size', 'Documentation', 'Code Quality']

        # Build matrix
        data_matrix = [
            [p['complexity_debt'] for p in programs],
            [p['coupling_debt'] for p in programs],
            [p['size_debt'] for p in programs],
            [p['documentation_debt'] for p in programs],
            [p['code_quality_debt'] for p in programs]
        ]

        fig = go.Figure(data=go.Heatmap(
            z=data_matrix,
            x=program_names,
            y=components,
            colorscale='RdYlGn_r',  # Red-Yellow-Green reversed
            hovertemplate='Program: %{x}<br>Component: %{y}<br>Score: %{z:.1f}<extra></extra>',
            colorbar=dict(title="Debt Score")
        ))

        fig.update_layout(
            title="Technical Debt Heatmap",
            xaxis_title="",
            yaxis_title="Debt Component",
            height=400,
            xaxis={'tickangle': -45}
        )

        return fig

    def create_scatter_size_vs_complexity(self, program_debts: List[Dict]) -> go.Figure:
        """
        Create scatter plot: Program size vs Complexity with debt severity

        Args:
            program_debts: List of program debt dictionaries

        Returns:
            Plotly Figure object
        """
        # Prepare data
        df = pd.DataFrame([
            {
                'Program': p['program_name'],
                'LOC': p['loc'],
                'Complexity': p['complexity_debt'],
                'Debt Score': p['total_debt_score'],
                'Severity': p['severity']
            }
            for p in program_debts
        ])

        fig = px.scatter(
            df,
            x='LOC',
            y='Complexity',
            size='Debt Score',
            color='Severity',
            color_discrete_map=self.SEVERITY_COLORS,
            hover_name='Program',
            hover_data={'LOC': True, 'Complexity': ':.1f', 'Debt Score': ':.1f'},
            labels={'LOC': 'Lines of Code', 'Complexity': 'Complexity Debt Score'}
        )

        fig.update_layout(
            title="Program Size vs Complexity (Bubble = Total Debt)",
            height=500
        )

        return fig

    def create_coupling_analysis_chart(self, program_debts: List[Dict], top_n: int = 15) -> go.Figure:
        """
        Create chart showing coupling metrics (calls in/out)

        Args:
            program_debts: List of program debt dictionaries
            top_n: Number of programs to show

        Returns:
            Plotly Figure object
        """
        # Get top N by coupling debt
        sorted_by_coupling = sorted(
            program_debts,
            key=lambda x: x['coupling_debt'],
            reverse=True
        )[:top_n]

        program_names = [p['program_name'] for p in sorted_by_coupling]
        calls_in = [p['calls_in'] for p in sorted_by_coupling]
        calls_out = [p['calls_out'] for p in sorted_by_coupling]

        fig = go.Figure()

        fig.add_trace(go.Bar(
            name='Called By (Dependencies)',
            y=program_names,
            x=calls_in,
            orientation='h',
            marker=dict(color='#3498db'),
            hovertemplate='Called by %{x} programs<extra></extra>'
        ))

        fig.add_trace(go.Bar(
            name='Calls (Dependencies)',
            y=program_names,
            x=calls_out,
            orientation='h',
            marker=dict(color='#e74c3c'),
            hovertemplate='Calls %{x} programs<extra></extra>'
        ))

        fig.update_layout(
            title=f"Coupling Analysis (Top {top_n} Programs)",
            xaxis_title="Number of Dependencies",
            yaxis_title="",
            barmode='group',
            height=max(400, top_n * 35),
            yaxis={'categoryorder': 'total ascending'}
        )

        return fig

    def create_summary_metrics_cards(self, debt_metrics: Dict) -> List[Dict]:
        """
        Create data for summary metric cards

        Args:
            debt_metrics: Overall debt metrics dictionary

        Returns:
            List of metric card dictionaries
        """
        avg_score = debt_metrics['avg_debt_score']

        # Determine overall health
        if avg_score >= 70:
            health = "Critical"
            health_color = "red"
        elif avg_score >= 50:
            health = "Poor"
            health_color = "orange"
        elif avg_score >= 30:
            health = "Fair"
            health_color = "yellow"
        else:
            health = "Good"
            health_color = "green"

        cards = [
            {
                'title': 'Total Programs',
                'value': debt_metrics['total_programs'],
                'icon': '📦'
            },
            {
                'title': 'Average Debt Score',
                'value': f"{avg_score:.1f}/100",
                'icon': '📊',
                'color': health_color
            },
            {
                'title': 'Overall Health',
                'value': health,
                'icon': '❤️',
                'color': health_color
            },
            {
                'title': 'Critical Programs',
                'value': debt_metrics['critical_count'],
                'icon': '⚠️',
                'color': 'red' if debt_metrics['critical_count'] > 0 else 'gray'
            },
            {
                'title': 'High Priority',
                'value': debt_metrics['high_count'],
                'icon': '🔴',
                'color': 'orange' if debt_metrics['high_count'] > 0 else 'gray'
            },
            {
                'title': 'Total LOC',
                'value': f"{debt_metrics['total_loc']:,}",
                'icon': '📝'
            }
        ]

        return cards


# Create singleton instance
tech_debt_visualizer = TechDebtVisualizer()
