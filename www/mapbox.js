// ============================================================
// MAP INITIALIZATION AND GLOBAL SETUP
// ============================================================

document.addEventListener("DOMContentLoaded", function () {
  // Set the Mapbox Access Token (assumed to be defined in app.R via tags$script)
  mapboxgl.accessToken = mapboxToken;

  // ------------------------------
  // Initialize the Map
  // ------------------------------
  // Create a new Mapbox map instance with the specified style, center (US), zoom level, max bounds, and projection.
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

  // ------------------------------
  // Add Navigation Controls
  // ------------------------------
  // Add built-in Mapbox navigation controls (zoom, rotate, etc.) to the map.
  map.addControl(new mapboxgl.NavigationControl());

  // ------------------------------
  // Define Global Variables
  // ------------------------------
  // searchPopup: holds the popup for search results.
  // hoverPopup: holds the popup shown when hovering over a feature.
  // hoveredFeatureId: stores the ID of the current feature being hovered.
  let searchPopup;
  let hoverPopup = new mapboxgl.Popup({ closeButton: false, closeOnClick: false });
  let hoveredFeatureId = null;

  // ============================================================
  // MAP LOAD EVENT: Add Legend, CZ Data, Mask Layer, etc.
  // ============================================================
  map.on("load", function () {
    // Add the legend to the map container.
    addLegend();

    // Load the Commuting Zone (CZ) data for a default year.
    loadCZDataForYear(2023);
    

    // --- Add Mask Layer ---
    // This layer covers areas outside the US by loading a precomputed GeoJSON mask.
    map.addSource("mask", {
      type: "geojson",
      data: "mask_polygon.geojson"  // File path relative to the www/ folder
    });
    map.addLayer({
      id: "mask-layer",
      type: "fill",
      source: "mask",
      paint: {
        "fill-color": "#ffffff",  // Color for the mask
        "fill-opacity": 1
      }
    });
    
    // Load the CZ data for the default year (2023) and add it to the map.
    //loadInstituteDataForYear(2023);
    loadInstituteDataForYear(2023);
    // Once all layers are loaded (i.e., the map is idle), make the map visible.
    map.on("idle", function() {
    // ensure the institutes layer is on top
    if (map.getLayer("institutes-layer")) {
      map.moveLayer("institutes-layer");
    }
    // then show the map
    document.getElementById("map").style.visibility = "visible";
  });
  
});

Shiny.addCustomMessageHandler("resizeMap", function(message) {
  // call the resize function on the map instance
  if (map) {
    map.resize();
  }
});

  // ============================================================
  // FUNCTION DEFINITIONS
  // ============================================================

  // ------------------------------
  // Function: addLegend
  // ------------------------------
  // Creates and styles an HTML legend for the map that explains the color ranges
  // for green job postings and appends it to the map container.
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
      '<div><span style="background-color: #edf8fb; width: 20px; height: 20px; display: inline-block; margin-right: 5px; border-radius: 0%;"></span>&lt; 152</div>' +
      '<div><span style="background-color: #b2e2e2; width: 20px; height: 20px; display: inline-block; margin-right: 5px; border-radius: 0%;"></span>152 – 489</div>' +
      '<div><span style="background-color: #66c2a4; width: 20px; height: 20px; display: inline-block; margin-right: 5px; border-radius: 0%;"></span>489 – 1409</div>' +
      '<div><span style="background-color: #2ca25f; width: 20px; height: 20px; display: inline-block; margin-right: 5px; border-radius: 0%;"></span>1409 – 4914</div>' +
      '<div><span style="background-color: #006d2c; width: 20px; height: 20px; display: inline-block; margin-right: 5px; border-radius: 0%;"></span>≥ 4914</div>';
      
    
    legend.innerHTML += `
    <h4 style="margin-top:10px;">Institutions<br/>Green Completion %</h4>
    <div>
      <span style="
        display:inline-block;
        width:8px; height:8px;
        background: rgba(178,34,34,0.5);
        border-radius:50%;
        margin-right:5px;
        vertical-align:middle;"></span>
      Low %
    </div>
    <div>
      <span style="
        display:inline-block;
        width:14px; height:14px;
        background: rgba(178,34,34,0.5);
        border-radius:50%;
        margin-right:5px;
        vertical-align:middle;"></span>
      High %
    </div>
  `;
    
    
    map.getContainer().appendChild(legend);
  }

  // ------------------------------
  // Function: loadCZDataForYear
  // ------------------------------
  // Loads the Commuting Zone (CZ) data for a given year.
  // It constructs the file name (e.g., "CZData_2023.json"), fetches the GeoJSON data,
  // adds the CZ layer with specified color and style properties, then updates the layer’s data.
  function loadCZDataForYear(year) {
    const czUrl = "CZData_" + year + ".json";  // e.g., "CZData_2023.json"
    fetch(czUrl, { cache: "force-cache" })
      .then(response => response.json())
      .then(function(data) {
        // Add the CZ layer if it does not exist.
        map.addLayer({
          id: "cz-layer",
          type: "fill",
          source: "cz",
          paint: {
            "fill-color": [
              "step",
              ["get", "green_job_postings"],
              "#edf8fb", 152,
              "#b2e2e2", 489,
              "#66c2a4", 1409,
              "#2ca25f", 4914,
              "#006d2c"
            ],
            "fill-opacity": [
              "case",
              ["boolean", ["feature-state", "hover"], false],
              1,
              0.7
            ],
            "fill-outline-color": "#000000"
          }
        });
        // Update the CZ data source with the fetched GeoJSON data.
        updateCZLayer(data);
      })
      .catch(function(error) {
        console.error("Error loading " + czUrl + ":", error);
      });
  }

  // ------------------------------
  // Function: updateCZLayer
  // ------------------------------
  // Updates the GeoJSON data for the Commuting Zone (CZ) layer.
  // If the source "cz" exists, its data is refreshed; otherwise, a new source is created.
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

  // ------------------------------
  // Function: loadInstituteDataForYear
  // ------------------------------
  // Loads institution data for a given year by fetching the corresponding GeoJSON file.
  // If the "institutes" source already exists, its data is updated; otherwise, a new source and layer are added.
  // The institution layer displays data as circles sized by the "inst_perc_green_tot" property.
  function loadInstituteDataForYear(year) {
    const instUrl = "InstituteData_" + year + ".json";  // e.g., "InstituteData_2024.json"
    fetch(instUrl, { cache: "force-cache" })
      .then(response => response.json())
      .then(function(data) {
        if (map.getSource("institutes")) {
          // Update existing source data.
          map.getSource("institutes").setData(data);
        } else {
          // Add new source for institutions.
          map.addSource("institutes", {
            type: "geojson",
            data: data
          });
          // Add a new circle layer for institutions with radius based on inst_perc_green_tot.
          map.addLayer({
            id: "institutes-layer",
            type: "circle",
            source: "institutes",
            paint: {
              "circle-radius": [
                "interpolate",
                ["linear"],
                ["get", "inst_perc_green_tot"],
                0, 2,
                1, 10
              ],
              "circle-color": "rgba(178, 34, 34, 0.5)",       // Adjust opacity(transparency) for better visibility
              "circle-stroke-width": 1,
              "circle-stroke-color": "#FFFFFF"
            }
          });
          
          
          // ------------------------------
          // Event: Institution Layer Click
          // ------------------------------
          // When a user clicks a circle on the institution layer, show a popup with the institution name
          // and its green completion percentage.
          map.on("click", "institutes-layer", function(e) {
            if (e.features.length > 0) {
              const feature = e.features[0];
              const perc = Number(feature.properties.inst_perc_green_tot);
              const percText = (perc * 100).toFixed(1) + "%";
              new mapboxgl.Popup()
                .setLngLat(e.lngLat)
                .setHTML("<strong>" + feature.properties.instnm + "</strong><br>Green Completion: " + percText)
                .addTo(map);
            }
          });
          // ------------------------------
          // Event: Change Cursor on Hover for Institution Layer
          // ------------------------------
          // Change the cursor style to "pointer" when hovering over the institution layer.
          map.on("mouseenter", "institutes-layer", function() {
            map.getCanvas().style.cursor = "pointer";
          });
          map.on("mouseleave", "institutes-layer", function() {
            map.getCanvas().style.cursor = "";
          });
        }
      })
      .catch(function(error) {
        console.error("Error loading " + instUrl + ":", error);
      });
  }

  // ============================================================
  // SHINY CUSTOM MESSAGE HANDLERS
  // ============================================================

  // ------------------------------
  // Handler: loadYear
  // ------------------------------
  // Receives the selected year from the Shiny server to load the corresponding CZ data.
  Shiny.addCustomMessageHandler("loadYear", function(year) {
    console.log("[loadYear] received:", year);
    loadCZDataForYear(year);
  });

  // ------------------------------
  // Handler: loadInstituteYear
  // ------------------------------
  // Receives the selected year from the Shiny server to load institution data.
  Shiny.addCustomMessageHandler("loadInstituteYear", function(year) {
    console.log("[loadInstituteYear] received:", year);
    loadInstituteDataForYear(year);
  });

  // ------------------------------
  // Handler: updateCZ
  // ------------------------------
  // Updates the CZ layer when new GeoJSON data is received from the Shiny server.
  Shiny.addCustomMessageHandler("updateCZ", function(czGeojson) {
    updateCZLayer(czGeojson);
  });

  // ------------------------------
  // Handler: updateSearch
  // ------------------------------
  // When a search result is sent from the Shiny server, display a popup at the specified location
  // and smoothly fly to that area with an increased zoom level.
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

  // ============================================================
  // MAP INTERACTION EVENTS (Commuting Zones)
  // ============================================================

  // ------------------------------
  // Event: Mouse Move on CZ Layer
  // ------------------------------
  // As the user moves the mouse over a commuting zone, update the feature state to reflect
  // the hover and display a popup with details (CZ identifier and green job postings count).
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

  // ------------------------------
  // Event: Mouse Leave from CZ Layer
  // ------------------------------
  // When the mouse leaves the commuting zone layer, remove the hover state and popup.
  map.on("mouseleave", "cz-layer", function() {
    if (hoveredFeatureId !== null) {
      map.setFeatureState({ source: "cz", id: hoveredFeatureId }, { hover: false });
    }
    hoveredFeatureId = null;
    hoverPopup.remove();
  });

  // ============================================================
  // GLOBAL UTILITY FUNCTIONS
  // ============================================================

  // ------------------------------
  // Function: clearMap
  // ------------------------------
  // This function, called by the Clear button, removes any search result popup and resets the map view
  // back to its default center and zoom level.
  window.clearMap = function () {
    if (searchPopup) {
      searchPopup.remove();
      searchPopup = null;
    }
    map.flyTo({ center: [-95, 40], zoom: 3.5 });
  };

});
