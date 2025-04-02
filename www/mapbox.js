document.addEventListener("DOMContentLoaded", function () {
  // Set the Mapbox Access Token (assumed to be defined in app.R via tags$script)
  mapboxgl.accessToken = mapboxToken;
  
  // Initialize the map using the navigation-day-v1 style,
  // centered on the US with a zoom level of 3.5,
  // with maximum bounds roughly covering the US,
  // and using the mercator (planar) projection.
  const map = new mapboxgl.Map({
    container: "map",
    style: "mapbox://styles/mapbox/navigation-day-v1",
    center: [-95, 40],
    zoom: 3.5,
    maxBounds: [
      [-130, 5],   // Southwest corner (roughly western and southern border)
      [-60, 60]    // Northeast corner (roughly eastern and northern border)
    ],
    projection: { name: "mercator" }
  });
  
  // Add navigation controls to the map
  map.addControl(new mapboxgl.NavigationControl());
  
  // Global variables: store the search popup, hover popup, and currently hovered feature ID
  let searchPopup;
  let hoverPopup = new mapboxgl.Popup({ closeButton: false, closeOnClick: false });
  let hoveredFeatureId = null;
  
  // When the map has finished loading, add the legend, mask layer, etc.
map.on("load", function () {
  addLegend();
  
  
  loadCZDataForYear("2024");
  // --- Add mask layer: Cover areas outside the US ---
  map.addSource("mask", {
    type: "geojson",
    data: "mask_polygon.geojson"  // Path relative to the www/ folder
  });
  
  map.addLayer({
    id: "mask-layer",
    type: "fill",
    source: "mask",
    paint: {
      "fill-color": "#ffffff",  // Adjust the color as needed
      "fill-opacity": 1.0
    }
  });
  
  // Once the map is idle (all layers loaded), make the map container visible
  map.on("idle", function() {
    document.getElementById("map").style.visibility = "visible";
  });
});

  
  // Function to add a legend to the map container
  function addLegend() {
    const legend = document.createElement("div");
    legend.id = "legend";
    legend.style.position = "absolute";
    legend.style.bottom = "30px";
    legend.style.right = "10px";
    legend.style.backgroundColor = "rgba(255, 255, 255, 0.8)";
    legend.style.padding = "10px";
    legend.style.fontFamily = "Arial, sans-serif";
    legend.style.fontSize = "12px";
    legend.style.boxShadow = "0 0 3px rgba(0,0,0,0.4)";
    legend.innerHTML = '<h4>Green Job Postings</h4>' +
      '<div><span style="background-color: #d0f3d0; width: 20px; height: 20px; display: inline-block; margin-right: 5px; border-radius: 50%;"></span>&lt; 100</div>' +
      '<div><span style="background-color: #a1e9a1; width: 20px; height: 20px; display: inline-block; margin-right: 5px; border-radius: 50%;"></span>100 - 500</div>' +
      '<div><span style="background-color: #99EA85; width: 20px; height: 20px; display: inline-block; margin-right: 5px; border-radius: 50%;"></span>500 - 1000</div>' +
      '<div><span style="background-color: #66c456; width: 20px; height: 20px; display: inline-block; margin-right: 5px; border-radius: 50%;"></span>&gt; 1000</div>';
    map.getContainer().appendChild(legend);
  }
  
  
    // Function to load CZ data for a given year using fetch
  function loadCZDataForYear(year) {
    const czUrl = "CZData_" + year + ".json";  // Construct file name, e.g., "CZData_2023.json"
    fetch(czUrl, { cache: "force-cache" })
      .then(response => response.json())
      .then(function(data) {
              map.addLayer({
        id: "cz-layer",
        type: "fill",
        source: "cz",
        paint: {
          // Define color steps for green_job_postings; adjust thresholds as needed
          "fill-color": [
            "step",
            ["get", "green_job_postings"],
            "#d0f3d0", 100,
            "#a1e9a1", 500,
            "#99EA85", 1000,
            "#66c456"
          ],
          "fill-opacity": [
            "case",
            ["boolean", ["feature-state", "hover"], false],
            1,
            0.8
          ],
          "fill-outline-color": "#ffffff"
        }
      });
        // Update or add the CZ layer using the fetched data
        updateCZLayer(data);
      })
      .catch(function(error) {
        console.error("Error loading " + czUrl + ":", error);
      });
  }
  
  // Shiny message handler: update CZ layer based on the provided GeoJSON data
  // This handler will be triggered when the user changes the year in the UI.
  Shiny.addCustomMessageHandler("loadYear", function(year) {
    console.log("Received year from server:", year);
    loadCZDataForYear(year);
  });
  
  // Function to update the commuting zone layer with CZ GeoJSON data
  function updateCZLayer(czGeojson) {
    if (map.getSource("cz")) {
      map.getSource("cz").setData(czGeojson);
    } else {
      map.addSource("cz", {
        type: "geojson",
        data: czGeojson
      });

    }
  }
  
  
  // Shiny message handler: update commuting zone layer based on the provided GeoJSON data
  Shiny.addCustomMessageHandler("updateCZ", function(czGeojson) {
    updateCZLayer(czGeojson);
  });
  // Mouse move event on the commuting zones layer: display commuting zone info
  map.on("mousemove", "cz-layer", function(e) {
    console.log("Mousemove on cz-layer:", e.features);
    if (e.features.length > 0) {
      if (hoveredFeatureId !== null) {
        map.setFeatureState({ source: "cz", id: hoveredFeatureId }, { hover: false });
      }
      hoveredFeatureId = e.features[0].id;
      map.setFeatureState({ source: "cz", id: hoveredFeatureId }, { hover: true });
      const properties = e.features[0].properties;
      hoverPopup
        .setLngLat(e.lngLat)
        .setHTML("<strong>CZ:</strong> " + properties.CZ20 + "<br><strong>Green Job Postings:</strong> " + properties.green_job_postings)
        .addTo(map);
    }
  });
  
  // Mouse leave event on the commuting zones layer: remove hover popup
  map.on("mouseleave", "cz-layer", function() {
    if (hoveredFeatureId !== null) {
      map.setFeatureState({ source: "cz", id: hoveredFeatureId }, { hover: false });
    }
    hoveredFeatureId = null;
    hoverPopup.remove();
  });
  
  // Shiny message handler for search results: display popup and fly to location
  Shiny.addCustomMessageHandler("updateSearch", function (coords) {
    if (searchPopup) {
      searchPopup.remove();
    }
    searchPopup = new mapboxgl.Popup()
      .setLngLat([coords.lng, coords.lat])
      .setHTML(coords.popup)
      .addTo(map);
    map.flyTo({ center: [coords.lng, coords.lat], zoom: 8 });
  });
  
  // Global function clearMap(), called directly by the Clear button
  window.clearMap = function () {
    if (searchPopup) {
      searchPopup.remove();
      searchPopup = null;
    }
    map.flyTo({ center: [-95, 40], zoom: 3.5 });
  };
});
