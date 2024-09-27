import plotly.graph_objects as go
import requests
import pandas as pd


def get_ip_location(ip_address):
    response = requests.get(f"https://ipapi.co/{ip_address}/json/")
    data = response.json()
    return data.get("latitude"), data.get("longitude"), data.get("country_name")


# Sample IP addresses
ip_addresses = ["1.1.1.1"]

# Collect data
data = []
for ip in ip_addresses:
    lat, lon, country = get_ip_location(ip)
    if lat and lon:
        data.append([ip, lat, lon, country])

# Create a DataFrame
df = pd.DataFrame(data, columns=["IP", "Latitude", "Longitude", "Country"])

# Create the map
fig = go.Figure(
    data=go.Scattergeo(
        lon=df["Longitude"],
        lat=df["Latitude"],
        text=df["IP"] + "<br>" + df["Country"],
        mode="markers",
        marker=dict(
            size=8,
            opacity=0.8,
            reversescale=True,
            autocolorscale=False,
            symbol="circle",
            line=dict(width=1, color="rgba(102, 102, 102)"),
            colorscale="Viridis",
            cmin=0,
            color=df.index,
            cmax=df.index.max(),
            colorbar_title="IP Addresses",
        ),
    )
)

fig.update_layout(
    title="IP Address Locations",
    geo=dict(
        scope="world",
        projection_type="natural earth",
        showland=True,
        landcolor="rgb(250, 250, 250)",
        subunitcolor="rgb(217, 217, 217)",
        countrycolor="rgb(217, 217, 217)",
        countrywidth=0.5,
        subunitwidth=0.5,
    ),
)

# Save the map as an interactive HTML file
fig.write_html(r"Practical\Medium\IP Tracking visualization\interactive_ip_map.html")

# Optionally, show the map in a browser
fig.show()
