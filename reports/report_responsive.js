
<script>
document.addEventListener('DOMContentLoaded', function() {
  // Resize Plotly graphs on window resize
  function resizePlotly() {
    document.querySelectorAll('.plotly').forEach(function(gd) {
      try { Plotly.Plots.resize(gd); } catch(e) { /* ignore */ }
    });
  }

  // Resize Leaflet maps (invalidateSize) on window resize
  function resizeLeaflet() {
    if(window.L && window.L.map) {
      // find leaflet containers and call invalidateSize on their maps if accessible
      document.querySelectorAll('.leaflet-container').forEach(function(el) {
        // If map object stored on element (shiny/leaflet may store), try to call invalidateSize
        if(el._leaflet_map && typeof el._leaflet_map.invalidateSize === 'function') {
          el._leaflet_map.invalidateSize();
        }
      });
    }
  }

  // Debounced resize handler
  var resizeTimer;
  window.addEventListener('resize', function() {
    clearTimeout(resizeTimer);
    resizeTimer = setTimeout(function() {
      resizePlotly();
      resizeLeaflet();
    }, 150);
  });

  // When Bootstrap tab shown (Rmd tabset), trigger resize for visible plots
  document.querySelectorAll('a[data-toggle="tab"]').forEach(function(tab) {
    tab.addEventListener('shown.bs.tab', function(e) {
      setTimeout(function(){ resizePlotly(); resizeLeaflet(); }, 120);
    });
  });

  // Also trigger once after load
  setTimeout(function(){ resizePlotly(); resizeLeaflet(); }, 300);
});
</script>

