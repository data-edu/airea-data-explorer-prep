// -----------------------------------------------------------------------------
// Interactive U.S. Green Jobs & Degrees Map (MapboxGL + Shiny)
// This script initializes the map, handles data loading/rendering for both
// Commuting Zones (CZ) and Institutes, manages pop-ups, hover states, legends,
// and listens for messages from the Shiny backend.
// -----------------------------------------------------------------------------

document.addEventListener("DOMContentLoaded", function () {
  // ──────────────────────────────────────────────────────────────────────────
  // 1) Map Initialization & Global State
  // ──────────────────────────────────────────────────────────────────────────
  mapboxgl.accessToken = mapboxToken;
  const map = new mapboxgl.Map({
    container: "map",
    style: "mapbox://styles/mapbox/navigation-day-v1",
    center: [-95, 40],
    zoom: 3.2,
    maxBounds: [[-130, 5], [-60, 60]],
    projection: { name: "albers" }
  });
  map.addControl(new mapboxgl.NavigationControl());
  let searchPopup;
  let hoverPopup = new mapboxgl.Popup({ closeButton: false, closeOnClick: false });
  let hoveredFeatureId = null;

  // ──────────────────────────────────────────────────────────────────────────
  // 2) "Color by" Helpers: Thresholds, Palettes, Labels
  // ──────────────────────────────────────────────────────────────────────────
  const czBreaks = {
    airea_job_posting: [99, 499, 999, 4999],
    pct_green:          [1,   5,    10,   20],
    per1000:            [2,   5,    10,   20]
  };
  const czColors = {
    airea_job_posting: ["#edf8fb","#b2e2e2","#66c2a4","#2ca25f","#006d2c"],
    pct_green:          ["#f7fcf5","#c7e9c0","#74c476","#238b45","#00441b"],
    per1000:            ["#fff5eb","#fdd0a2","#f16913","#d94801","#8c2d04"]
  };
  const czLabels = {
    airea_job_posting: "AIREA Job Postings",
    pct_green:          "AIREA Percentage ",
    per1000:            "Jobs per 1,000"
  };
  // Currently selected metric (defaults to "airea_job_posting")
  let currentCZMetric = "airea_job_posting";

  function buildFillColorExpr(metric) {
    const expr = ["step", ["get", metric], czColors[metric][0]];
    czBreaks[metric].forEach((b, i) => {
      expr.push(b, czColors[metric][i+1]);
    });
    return expr;
  }
  
  function applyCZPaint(metric) {
    map.setPaintProperty("cz-layer", "fill-color", buildFillColorExpr(metric));
    map.setPaintProperty("cz-layer", "fill-opacity", [
      "case", ["boolean", ["feature-state","hover"], false],
      1, .7
    ]);
    map.setPaintProperty("cz-layer", "fill-outline-color", "#000000");

    // **Only refresh the upper part** of the CZ legend; the lower part remains unchanged
    updateCZLegend(metric);
  }

 // ──────────────────────────────────────────────────────────────────────────
 // 3) Legend Helper
 // ──────────────────────────────────────────────────────────────────────────

  function createCombinedLegend() {
    // If legend already exists, skip
    if (document.getElementById("legend")) return;

    const legend = document.createElement("div");
    legend.id = "legend";
    Object.assign(legend.style, {
      position:      "absolute",
      bottom:        "30px",
      right:         "10px",
      backgroundColor: "rgba(255,255,255,0.8)",
      padding:       "10px",
      fontFamily:    "Arial, sans-serif",
      fontSize:      "12px",
      boxShadow:     "0 0 3px rgba(0,0,0,0.4)",
      borderRadius:  "4px",
      zIndex:        1
    });

    // Upper section: CZ legend (refreshed later by updateCZLegend)
    const czSection = document.createElement("div");
    czSection.id = "legend-cz";
    legend.appendChild(czSection);

    // Lower section: Static AIREA Degree Rate legend
    const instSection = document.createElement("div");
    instSection.id = "legend-inst";
    instSection.innerHTML = `
      <h4 style="margin:10px 0 5px;">AIREA Degree Rate</h4>
      <div><span style="background:rgba(178,34,34,0.5);width:8px;height:8px;
                   display:inline-block;border-radius:50%;margin-right:5px;vertical-align:middle;"></span>
        Low < 25%</div>
      <div><span style="background:rgba(178,34,34,0.5);width:10px;height:10px;
                   display:inline-block;border-radius:50%;margin-right:5px;vertical-align:middle;"></span>
        25% – 50%</div>
      <div><span style="background:rgba(178,34,34,0.5);width:12px;height:12px;
                   display:inline-block;border-radius:50%;margin-right:5px;vertical-align:middle;"></span>
        50% – 75%</div>
      <div><span style="background:rgba(178,34,34,0.5);width:14px;height:14px;
                   display:inline-block;border-radius:50%;margin-right:5px;vertical-align:middle;"></span>
        ≥ 75%</div>
    `;
    legend.appendChild(instSection);

    map.getContainer().appendChild(legend);
  }

  function updateCZLegend(metric) {
    const czSection = document.getElementById("legend-cz");
    const breaks = czBreaks[metric];
    const colors = czColors[metric];
    const title  = czLabels[metric];
    
    // The upper section HTML
    let html = `<h4 style="margin:0 0 5px;">${title}</h4>`;
    // First range
    html += `
      <div>
        <span style="background:${colors[0]};width:20px;height:20px;
                     display:inline-block;margin-right:5px;"></span>
        < ${breaks[0]}
      </div>`;
    // Middle ranges
    for (let i = 0; i < breaks.length - 1; i++) {
      html += `
        <div>
          <span style="background:${colors[i+1]};width:20px;height:20px;
                       display:inline-block;margin-right:5px;"></span>
          ${breaks[i]} – ${breaks[i+1]}
        </div>`;
    }
    // Last range
    html += `
      <div>
        <span style="background:${colors[colors.length-1]};width:20px;height:20px;
                     display:inline-block;margin-right:5px;"></span>
        ≥ ${breaks[breaks.length-1]}
      </div>`;

    czSection.innerHTML = html;
  }


  // ──────────────────────────────────────────────────────────────────────────
  // 4) Map Loaded: Add Mask Layer & Initial Legend
  // ──────────────────────────────────────────────────────────────────────────
  map.on("load", function () {
    // Mask layer
    map.addSource("mask", {
      type: "geojson", data: "mask_polygon.geojson"
    });
    map.addLayer({
      id: "mask-layer", type: "fill", source: "mask",
      paint: { "fill-color": "#ffffff", "fill-opacity": 1 }
    });
    // Ensure layer order
    map.on("idle", function () {
      if (map.getLayer("cz-layer"))         map.moveLayer("cz-layer");
      if (map.getLayer("institutes-layer")) map.moveLayer("institutes-layer");
      document.getElementById("map").style.visibility = "visible";
    });
    
    createCombinedLegend();
    updateCZLegend(currentCZMetric);  // Initial rendering of the CZ section
  });

  // ──────────────────────────────────────────────────────────────────────────
  // 5) Shiny → JS Message Handling
  // ──────────────────────────────────────────────────────────────────────────
  Shiny.addCustomMessageHandler("loadYear", function(year) {
    loadCZDataForYear(year);
  });
  Shiny.addCustomMessageHandler("loadInstituteYear", function(year) {
    loadInstituteDataForYear(year);
  });
  Shiny.addCustomMessageHandler("updateSearch", function(coords) {
    if (searchPopup) searchPopup.remove();
    searchPopup = new mapboxgl.Popup()
      .setLngLat([coords.lng, coords.lat])
      .setHTML(coords.popup)
      .addTo(map);
    map.flyTo({ center: [coords.lng, coords.lat], zoom: 8 });
  });
  // New: listen for color-by backend messages
  Shiny.addCustomMessageHandler("updateCZMetric", function(metric) {
    currentCZMetric = metric;
    if (map.getLayer("cz-layer")) applyCZPaint(metric);
  });

  // ──────────────────────────────────────────────────────────────────────────
  // 6) CZ Data Loading & Layer Update
  // ──────────────────────────────────────────────────────────────────────────
function loadCZDataForYear(year) {
  const url = `CZData_${year}.json`;
  fetch(url, { cache: "force-cache" })
    .then(r => r.json())
    .then(data => {
      if (!map.getSource("cz")) {
        map.addSource("cz", {
          type:       "geojson",
          data:       data,
          generateId: true
        });
        
        map.addLayer({ id: "cz-layer", type: "fill", source: "cz"  });
        map.on("mousemove", "cz-layer", onCZMouseMove);
        map.on("mouseleave","cz-layer", onCZMouseLeave);
      } else {
        map.getSource("cz").setData(data);
      }
      applyCZPaint(currentCZMetric);
    })
    .catch(err => console.error("Error loading " + url, err));
}

  function onCZMouseMove(e) {
    if (!e.features.length) return;
    if (hoveredFeatureId !== null) {
      map.setFeatureState({ source: "cz", id: hoveredFeatureId }, { hover: false });
    }
    hoveredFeatureId = e.features[0].id;
    map.setFeatureState({ source: "cz", id: hoveredFeatureId }, { hover: true });
  }
  function onCZMouseLeave() {
    if (hoveredFeatureId !== null) {
      map.setFeatureState({ source: "cz", id: hoveredFeatureId }, { hover: false });
      hoveredFeatureId = null;
    }
   
  }

  // ──────────────────────────────────────────────────────────────────────────
  // 7) Institute Data Loading
  // ──────────────────────────────────────────────────────────────────────────
// global hover popup for institutes
let instHoverPopup = new mapboxgl.Popup({
  closeButton: false,
  closeOnClick: false
});

function loadInstituteDataForYear(year) {
  const url = `InstituteData_${year}.json`;
  fetch(url, { cache: "force-cache" })
    .then(r => r.json())
    .then(data => {
      if (!map.getSource("institutes")) {
        map.addSource("institutes", { type: "geojson", data });

        map.addLayer({
          id: "institutes-layer",
          type: "circle",
          source: "institutes",
          paint: {
            "circle-radius": [
              "interpolate", ["linear"],
              ["get", "inst_perc_green_tot"],
              0, 2,
              1, 10
            ],
            "circle-color":       "rgba(178,34,34,0.5)",
            "circle-stroke-width": 1,
            "circle-stroke-color": "#ffffff"
          }
        });

        // --- Hover for institutes-layer ---
        map.on("mouseenter", "institutes-layer", function(e) {
          map.getCanvas().style.cursor = "pointer";
          if (!e.features.length) return;

          const props = e.features[0].properties;
          const coords = e.lngLat;

          const html = `
            <strong>${props.instnm}</strong><br>
            ${props.CZ_label}<br>
            AIREA Degrees: ${(+props.inst_green_cmplt_tot).toLocaleString()}<br>
            Rate: ${(props.inst_perc_green_tot*100).toFixed(1)}%
          `;

          instHoverPopup
            .setLngLat(coords)
            .setHTML(html)
            .addTo(map);
        });

        map.on("mouseleave", "institutes-layer", function() {
          map.getCanvas().style.cursor = "";
          instHoverPopup.remove();
        });

      } else {
        map.getSource("institutes").setData(data);
      }

      // refresh the institute legend
      addInstituteLegend();
    })
    .catch(err => console.error("Error loading " + url, err));
}

  // ──────────────────────────────────────────────────────────────────────────
  // 8) Clear Map (Search)
  // ──────────────────────────────────────────────────────────────────────────
  window.clearMap = function () {
    if (searchPopup) { searchPopup.remove(); searchPopup = null; }
    map.flyTo({ center: [-95, 40], zoom: 3.5 });
  };

});