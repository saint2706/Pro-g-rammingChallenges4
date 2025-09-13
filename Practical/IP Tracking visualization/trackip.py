import plotly.graph_objects as go
import requests
import pandas as pd
import argparse
import sys
import os
from typing import List, Dict, Any, Optional

# Add dependency checks for a better user experience.
try:
    from tqdm import tqdm
except ImportError:
    print("Error: The 'tqdm' library is required for the progress bar.", file=sys.stderr)
    print("Please install it using: pip install tqdm", file=sys.stderr)
    sys.exit(1)

def fetch_ip_locations(ip_addresses: List[str]) -> List[Dict[str, Any]]:
    """
    Fetches geographic location data for a list of IP addresses from the ipapi.co API.

    Args:
        ip_addresses: A list of IP address strings.

    Returns:
        A list of dictionaries, where each dictionary contains the location
        data for a successfully located IP address.
    """
    location_data = []
    print(f"Fetching location data for {len(ip_addresses)} IP address(es)...")

    for ip in tqdm(ip_addresses, desc="Querying API"):
        try:
            response = requests.get(f"https://ipapi.co/{ip}/json/", timeout=5)
            response.raise_for_status()
            data = response.json()

            if data.get("error"):
                print(f"Warning: Could not locate IP {ip}. Reason: {data.get('reason')}", file=sys.stderr)
                continue

            if all(k in data for k in ["latitude", "longitude", "country_name"]):
                location_data.append({
                    "IP": ip,
                    "Latitude": data["latitude"],
                    "Longitude": data["longitude"],
                    "Country": data["country_name"],
                    "City": data.get("city", "N/A"),
                    "Org": data.get("org", "N/A")
                })
        except requests.exceptions.RequestException as e:
            print(f"Warning: Network error for IP {ip}: {e}", file=sys.stderr)
        except Exception as e:
            print(f"Warning: An unexpected error occurred for IP {ip}: {e}", file=sys.stderr)

    return location_data

def create_map(location_data: List[Dict[str, Any]], output_path: str):
    """
    Creates and saves an interactive world map of IP locations using Plotly.

    Args:
        location_data: A list of dictionaries containing location data.
        output_path: The path to save the output HTML file.
    """
    if not location_data:
        print("No valid location data to plot.")
        return

    df = pd.DataFrame(location_data)

    fig = go.Figure(data=go.Scattergeo(
        lon=df["Longitude"],
        lat=df["Latitude"],
        text=df["IP"] + "<br>" + df["City"] + ", " + df["Country"] + "<br>" + df["Org"],
        mode="markers",
        marker=dict(
            size=8,
            opacity=0.8,
            symbol="circle",
            line=dict(width=1, color="rgba(102, 102, 102)"),
            color="blue",
        ),
    ))

    fig.update_layout(
        title="IP Address Geographic Locations",
        geo=dict(
            scope="world",
            projection_type="natural earth",
            showland=True,
            landcolor="rgb(243, 243, 243)",
            countrycolor="rgb(204, 204, 204)",
        ),
    )

    try:
        fig.write_html(output_path)
        print(f"\nMap successfully saved to '{os.path.abspath(output_path)}'")
    except Exception as e:
        print(f"Error saving map to HTML: {e}", file=sys.stderr)

def main():
    """Main function to parse arguments and run the IP tracker."""
    parser = argparse.ArgumentParser(description="Visualize the geographic location of IP addresses on a world map.")
    parser.add_argument("ips", nargs='+', help="One or more IP addresses to track.")
    parser.add_argument("-o", "--output", default="ip_map.html",
                        help="Output HTML file name for the map. Defaults to 'ip_map.html'.")

    args = parser.parse_args()

    locations = fetch_ip_locations(args.ips)
    create_map(locations, args.output)

if __name__ == "__main__":
    main()
