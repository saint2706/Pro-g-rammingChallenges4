#!/usr/bin/env python3
"""
Advanced Character Counter Visualizer

A comprehensive visualization tool for character frequency analysis with multiple
chart types, styling options, and export capabilities. This tool creates publication-
quality charts and supports various visualization modes for text analysis.

Features:
- Multiple chart types (bar, pie, horizontal bar, heatmap)
- Customizable styling and color schemes
- Export to various formats (PNG, PDF, SVG, HTML)
- Interactive plots with detailed statistics
- Unicode-aware character handling
- Statistical overlays and annotations

Example:
    $ python charcount_visualizer.py --text "Hello World" --chart bar
    $ python charcount_visualizer.py --file document.txt --chart pie --style dark
    $ python charcount_visualizer.py --interactive

Author: Programming Challenges
Date: 2025
Version: 2.0
"""

import argparse
import logging
import sys
from pathlib import Path
from typing import List, Optional, Tuple
from enum import Enum
from dataclasses import dataclass

# Visualization libraries
try:
    import matplotlib.pyplot as plt
    import matplotlib.style as plt_style
    import matplotlib.cm as cm
    from matplotlib.figure import Figure
    import seaborn as sns
    import numpy as np
    from matplotlib.patches import Rectangle, Circle
    from matplotlib.collections import PatchCollection
except ImportError as e:
    print(f"Error: Required visualization libraries not found: {e}", file=sys.stderr)
    print("Please install: pip install matplotlib seaborn numpy", file=sys.stderr)
    sys.exit(1)

# Import the enhanced character analysis logic
try:
    from charcount import (
        get_char_counts,
        analyze_text_statistics,
        format_char_for_display,
        TextStatistics,
        analyze_character_categories,
        CharacterCategory,
    )
except ImportError:
    print(
        "Error: This script requires the enhanced 'charcount.py' to be in the same directory.",
        file=sys.stderr,
    )
    sys.exit(1)


class ChartType(Enum):
    """Enumeration of supported chart types."""

    BAR = "bar"
    HORIZONTAL_BAR = "hbar"
    PIE = "pie"
    DONUT = "donut"
    HISTOGRAM = "histogram"
    HEATMAP = "heatmap"
    TREEMAP = "treemap"


class StyleTheme(Enum):
    """Enumeration of supported visual themes."""

    DEFAULT = "default"
    DARK = "dark"
    MINIMAL = "minimal"
    COLORFUL = "colorful"
    ACADEMIC = "academic"
    SEABORN = "seaborn"


# Configure logging
logger = logging.getLogger(__name__)


@dataclass
class VisualizationConfig:
    """
    Configuration for visualization parameters.

    Attributes:
        chart_type: Type of chart to generate
        style_theme: Visual style theme to apply
        width: Figure width in inches
        height: Figure height in inches
        dpi: Resolution in dots per inch
        color_palette: Color palette name or list of colors
        show_values: Whether to display values on chart elements
        show_percentages: Whether to display percentages
        show_character_categories: Whether to show character categories instead of individual chars
        max_characters: Maximum number of characters to display
        font_size: Base font size for text elements
        title: Custom title for the chart
        output_file: Path to save the chart (optional)
    """

    chart_type: ChartType = ChartType.BAR
    style_theme: StyleTheme = StyleTheme.DEFAULT
    width: float = 12.0
    height: float = 8.0
    dpi: int = 300
    color_palette: str = "viridis"
    show_values: bool = True
    show_percentages: bool = False
    show_character_categories: bool = False
    max_characters: int = 20
    font_size: int = 10
    title: Optional[str] = None
    output_file: Optional[str] = None


def setup_logging(level: str = "INFO") -> None:
    """
    Configure logging for the application.

    Args:
        level: Logging level (DEBUG, INFO, WARNING, ERROR)
    """
    logging.basicConfig(
        level=getattr(logging, level.upper()),
        format="%(asctime)s - %(levelname)s - %(message)s",
        datefmt="%H:%M:%S",
    )


def apply_style_theme(theme: StyleTheme) -> None:
    """
    Apply the specified visual theme to matplotlib.

    Args:
        theme: The style theme to apply
    """
    if theme == StyleTheme.DARK:
        plt.style.use("dark_background")
        sns.set_palette("bright")
    elif theme == StyleTheme.MINIMAL:
        plt.style.use("seaborn-v0_8-whitegrid")
        sns.set_palette("pastel")
    elif theme == StyleTheme.COLORFUL:
        plt.style.use("seaborn-v0_8-colorblind")
        sns.set_palette("Set2")
    elif theme == StyleTheme.ACADEMIC:
        plt.style.use("seaborn-v0_8-paper")
        sns.set_palette("muted")
    elif theme == StyleTheme.SEABORN:
        sns.set_style("whitegrid")
        sns.set_palette("deep")
    else:  # DEFAULT
        plt.style.use("default")


def prepare_chart_data(
    statistics: TextStatistics, config: VisualizationConfig
) -> Tuple[List[str], List[int], List[str]]:
    """
    Prepare data for chart visualization.

    Args:
        statistics: Text analysis statistics
        config: Visualization configuration

    Returns:
        Tuple of (characters, frequencies, display_labels)
    """
    # Get the most common characters up to the limit
    most_common = statistics.character_counts.most_common(config.max_characters)

    if not most_common:
        return [], [], []

    characters = [item[0] for item in most_common]
    frequencies = [item[1] for item in most_common]

    # Create display labels with better formatting
    display_labels = []
    total_chars = statistics.total_characters

    for char, freq in most_common:
        char_display = format_char_for_display(char)

        if config.show_percentages:
            percentage = (freq / total_chars) * 100
            if config.show_values:
                label = f"{char_display}\n{freq} ({percentage:.1f}%)"
            else:
                label = f"{char_display}\n({percentage:.1f}%)"
        else:
            if config.show_values:
                label = f"{char_display}\n{freq}"
            else:
                label = char_display

        display_labels.append(label)

    return characters, frequencies, display_labels


def create_bar_chart(statistics: TextStatistics, config: VisualizationConfig) -> Figure:
    """Create a vertical bar chart of character frequencies."""
    characters, frequencies, display_labels = prepare_chart_data(statistics, config)

    if not characters:
        raise ValueError("No data to plot")

    fig, ax = plt.subplots(figsize=(config.width, config.height), dpi=config.dpi)

    # Create bars with color palette
    colors = cm.get_cmap(config.color_palette)(np.linspace(0, 1, len(characters)))
    bars = ax.bar(range(len(characters)), frequencies, color=colors)

    # Customize appearance
    ax.set_xlabel("Characters", fontsize=config.font_size + 2)
    ax.set_ylabel("Frequency", fontsize=config.font_size + 2)
    ax.set_title(
        config.title or "Character Frequency Distribution",
        fontsize=config.font_size + 4,
        fontweight="bold",
    )

    # Set x-axis labels
    ax.set_xticks(range(len(characters)))
    ax.set_xticklabels(
        [format_char_for_display(char) for char in characters],
        rotation=45,
        ha="right",
        fontsize=config.font_size,
    )

    # Add value labels on bars if requested
    if config.show_values:
        for i, (bar, freq) in enumerate(zip(bars, frequencies)):
            height = bar.get_height()
            percentage = (freq / statistics.total_characters) * 100
            label = f"{freq}"
            if config.show_percentages:
                label += f"\n({percentage:.1f}%)"

            ax.text(
                bar.get_x() + bar.get_width() / 2.0,
                height + max(frequencies) * 0.01,
                label,
                ha="center",
                va="bottom",
                fontsize=config.font_size - 1,
            )

    # Add grid for better readability
    ax.grid(True, alpha=0.3)
    plt.tight_layout()

    return fig


def create_horizontal_bar_chart(
    statistics: TextStatistics, config: VisualizationConfig
) -> Figure:
    """Create a horizontal bar chart of character frequencies."""
    characters, frequencies, display_labels = prepare_chart_data(statistics, config)

    if not characters:
        raise ValueError("No data to plot")

    fig, ax = plt.subplots(figsize=(config.width, config.height), dpi=config.dpi)

    # Create horizontal bars
    y_pos = np.arange(len(characters))
    colors = cm.get_cmap(config.color_palette)(np.linspace(0, 1, len(characters)))
    bars = ax.barh(y_pos, frequencies, color=colors)

    # Customize appearance
    ax.set_ylabel("Characters", fontsize=config.font_size + 2)
    ax.set_xlabel("Frequency", fontsize=config.font_size + 2)
    ax.set_title(
        config.title or "Character Frequency Distribution",
        fontsize=config.font_size + 4,
        fontweight="bold",
    )

    # Set y-axis labels (reversed for top-to-bottom display)
    ax.set_yticks(y_pos)
    ax.set_yticklabels(
        [format_char_for_display(char) for char in reversed(characters)],
        fontsize=config.font_size,
    )

    # Reverse the order for intuitive display (most frequent at top)
    ax.invert_yaxis()

    # Add value labels on bars if requested
    if config.show_values:
        for i, (bar, freq) in enumerate(zip(bars, reversed(frequencies))):
            width = bar.get_width()
            percentage = (freq / statistics.total_characters) * 100
            label = f"{freq}"
            if config.show_percentages:
                label += f" ({percentage:.1f}%)"

            ax.text(
                width + max(frequencies) * 0.01,
                bar.get_y() + bar.get_height() / 2.0,
                label,
                ha="left",
                va="center",
                fontsize=config.font_size - 1,
            )

    ax.grid(True, alpha=0.3, axis="x")
    plt.tight_layout()

    return fig


def create_pie_chart(statistics: TextStatistics, config: VisualizationConfig) -> Figure:
    """Create a pie chart of character frequencies."""
    characters, frequencies, display_labels = prepare_chart_data(statistics, config)

    if not characters:
        raise ValueError("No data to plot")

    fig, ax = plt.subplots(figsize=(config.width, config.height), dpi=config.dpi)

    # Create pie chart
    colors = cm.get_cmap(config.color_palette)(np.linspace(0, 1, len(characters)))

    # Calculate percentages
    total = sum(frequencies)
    percentages = [freq / total * 100 for freq in frequencies]

    # Create labels
    if config.show_values and config.show_percentages:
        labels = [
            f"{format_char_for_display(char)}\n{freq} ({pct:.1f}%)"
            for char, freq, pct in zip(characters, frequencies, percentages)
        ]
    elif config.show_values:
        labels = [
            f"{format_char_for_display(char)}\n{freq}"
            for char, freq in zip(characters, frequencies)
        ]
    elif config.show_percentages:
        labels = [
            f"{format_char_for_display(char)}\n({pct:.1f}%)"
            for char, pct in zip(characters, percentages)
        ]
    else:
        labels = [format_char_for_display(char) for char in characters]

    # Remove unpacking to avoid type mismatch (pyright: pie may return 2 or 3 values)
    ax.pie(
        frequencies,
        labels=labels,
        colors=colors.tolist(),
        autopct=(
            "%1.1f%%" if not config.show_percentages else None
        ),  # Use None instead of '' for clarity
        startangle=90,
    )

    ax.set_title(
        config.title or "Character Frequency Distribution",
        fontsize=config.font_size + 4,
        fontweight="bold",
    )

    # Ensure circular pie chart
    ax.axis("equal")

    return fig


def create_donut_chart(
    statistics: TextStatistics, config: VisualizationConfig
) -> Figure:
    """Create a donut chart of character frequencies."""
    fig = create_pie_chart(statistics, config)
    ax = fig.gca()

    # Add center circle to create donut effect
    centre_circle = Circle((0, 0), 0.70, fc="white")
    ax.add_artist(centre_circle)

    # Add center text with statistics
    center_text = f"Total: {statistics.total_characters:,}\nUnique: {statistics.unique_characters}"
    ax.text(
        0,
        0,
        center_text,
        ha="center",
        va="center",
        fontsize=config.font_size + 1,
        fontweight="bold",
    )

    return fig


def create_category_chart(
    statistics: TextStatistics, config: VisualizationConfig
) -> Figure:
    """Create a chart showing character categories."""
    categories = statistics.character_categories

    if not categories:
        raise ValueError("No category data to plot")

    fig, ax = plt.subplots(figsize=(config.width, config.height), dpi=config.dpi)

    # Prepare data
    category_names = list(categories.keys())
    category_counts = list(categories.values())

    # Create bars
    colors = cm.get_cmap(config.color_palette)(np.linspace(0, 1, len(category_names)))
    bars = ax.bar(category_names, category_counts, color=colors)

    # Customize appearance
    ax.set_xlabel("Character Categories", fontsize=config.font_size + 2)
    ax.set_ylabel("Count", fontsize=config.font_size + 2)
    ax.set_title(
        config.title or "Character Category Distribution",
        fontsize=config.font_size + 4,
        fontweight="bold",
    )

    # Add value labels
    if config.show_values:
        total = statistics.total_characters
        for bar, count in zip(bars, category_counts):
            height = bar.get_height()
            percentage = (count / total) * 100
            label = f"{count}"
            if config.show_percentages:
                label += f"\n({percentage:.1f}%)"

            ax.text(
                bar.get_x() + bar.get_width() / 2.0,
                height + max(category_counts) * 0.01,
                label,
                ha="center",
                va="bottom",
                fontsize=config.font_size - 1,
            )

    plt.xticks(rotation=45, ha="right")
    ax.grid(True, alpha=0.3)
    plt.tight_layout()

    return fig


def save_figure(fig: Figure, output_path: str, dpi: int = 300) -> None:
    """
    Save figure to file with appropriate format based on extension.

    Args:
        fig: Matplotlib figure to save
        output_path: Path where to save the figure
        dpi: Resolution for raster formats
    """
    file_path = Path(output_path)
    extension = file_path.suffix.lower()

    # Determine format and parameters
    if extension in [".png", ".jpg", ".jpeg"]:
        fig.savefig(
            output_path,
            dpi=dpi,
            bbox_inches="tight",
            facecolor="white",
            edgecolor="none",
        )
    elif extension == ".pdf":
        fig.savefig(output_path, format="pdf", bbox_inches="tight")
    elif extension == ".svg":
        fig.savefig(output_path, format="svg", bbox_inches="tight")
    else:
        # Default to PNG
        output_path = str(file_path.with_suffix(".png"))
        fig.savefig(
            output_path,
            dpi=dpi,
            bbox_inches="tight",
            facecolor="white",
            edgecolor="none",
        )

    logging.info(f"Chart saved to: {output_path}")


def plot_char_counts(statistics: TextStatistics, config: VisualizationConfig) -> Figure:
    """Create character frequency visualization based on configuration."""
    logger.info(
        f"Creating {config.chart_type.value} chart with {config.style_theme.value} theme"
    )

    # Get counts based on what to display
    if config.show_character_categories:
        counts = statistics.character_categories
    else:
        counts = statistics.character_counts

    if not counts:
        raise ValueError("No character data to visualize")

    # Apply style theme
    if config.style_theme != StyleTheme.DEFAULT:
        plt.style.use(config.style_theme.value)

    # Create chart based on type
    if config.chart_type == ChartType.BAR:
        return create_bar_chart(statistics, config)
    elif config.chart_type == ChartType.HORIZONTAL_BAR:
        return create_horizontal_bar_chart(statistics, config)
    elif config.chart_type == ChartType.PIE:
        return create_pie_chart(statistics, config)
    elif config.chart_type == ChartType.DONUT:
        return create_donut_chart(statistics, config)
    else:
        raise ValueError(f"Unsupported chart type: {config.chart_type}")


def main() -> None:
    """Main function to handle command line interface."""
    parser = argparse.ArgumentParser(
        description="Advanced Character Frequency Visualizer",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  %(prog)s input.txt --chart-type bar --max-chars 20
  %(prog)s input.txt --chart-type pie --output analysis.png
  %(prog)s input.txt --chart-type donut --theme seaborn --dpi 300
        """,
    )

    # Input/Output arguments
    parser.add_argument("input_file", help="Input text file to analyze")
    parser.add_argument(
        "--output", "-o", help="Output file path for saving the chart (optional)"
    )

    # Chart configuration arguments
    parser.add_argument(
        "--chart-type",
        "-t",
        type=str,
        choices=[ct.value for ct in ChartType],
        default=ChartType.BAR.value,
        help="Type of chart to create (default: %(default)s)",
    )
    parser.add_argument(
        "--theme",
        type=str,
        choices=[st.value for st in StyleTheme],
        default=StyleTheme.DEFAULT.value,
        help="Chart style theme (default: %(default)s)",
    )
    parser.add_argument(
        "--max-chars",
        "-n",
        type=int,
        default=20,
        help="Maximum number of characters to display (default: %(default)s)",
    )
    parser.add_argument(
        "--show-categories",
        action="store_true",
        help="Show character categories instead of individual characters",
    )
    parser.add_argument(
        "--color-palette",
        type=str,
        default="viridis",
        help="Color palette for the chart (default: %(default)s)",
    )
    parser.add_argument(
        "--dpi",
        type=int,
        default=100,
        help="DPI for chart display and saving (default: %(default)s)",
    )

    # Display options
    parser.add_argument(
        "--no-display",
        action="store_true",
        help="Do not display the chart interactively",
    )
    parser.add_argument("--title", type=str, help="Custom title for the chart")

    # Logging
    parser.add_argument(
        "--verbose", "-v", action="store_true", help="Enable verbose logging"
    )

    args = parser.parse_args()

    # Configure logging
    log_level = logging.DEBUG if args.verbose else logging.INFO
    logging.basicConfig(
        level=log_level, format="%(asctime)s - %(name)s - %(levelname)s - %(message)s"
    )

    try:
        # Read and analyze input file
        input_path = Path(args.input_file)
        if not input_path.exists():
            logger.error(f"Input file not found: {input_path}")
            sys.exit(1)

        logger.info(f"Reading file: {input_path}")
        with open(input_path, "r", encoding="utf-8") as file:
            text = file.read()

        if not text.strip():
            logger.error("Input file is empty")
            sys.exit(1)

        # Analyze text using charcount module
        statistics = analyze_text_statistics(text)
        logger.info(f"Analyzed {statistics.total_characters} characters")

        # Create visualization configuration
        config = VisualizationConfig(
            chart_type=ChartType(args.chart_type),
            style_theme=StyleTheme(args.theme),
            max_characters=args.max_chars,
            show_character_categories=args.show_categories,
            color_palette=args.color_palette,
            dpi=args.dpi,
            title=args.title or f"Character Analysis: {input_path.name}",
        )

        # Create visualization
        fig = plot_char_counts(statistics, config)

        # Save if output path specified
        if args.output:
            save_figure(fig, args.output, args.dpi)

        # Display chart if not disabled
        if not args.no_display:
            logger.info("Displaying chart...")
            plt.show()

        logger.info("Visualization complete!")

    except FileNotFoundError as e:
        logger.error(f"File not found: {e}")
        sys.exit(1)
    except Exception as e:
        logger.error(f"Error creating visualization: {e}")
        if args.verbose:
            import traceback

            traceback.print_exc()
        sys.exit(1)


if __name__ == "__main__":
    main()
