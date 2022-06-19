// This helper rewrites links to work on IPFS.
//
// The site normally serves page resources without an extension (in
// theory, resources could be served in alternate representations).
//
// However, IPFS can't really handle this, so we dynamically rewrite
// links to add a .html extension.
(function(){
  // Only run if this is on IPFS.
  if (!(window.location.href.includes("ipfs")
        || window.location.href.includes("ipns"))) {
    return;
  }
  window.addEventListener('load', function() {
    let elems = document.getElementsByTagName('a');
    for (let elem of elems) {
      let v = elem.attributes.href.value;
      if (v.endsWith('/')) {
        continue;
      }
      // Only handle links on this site.
      if (v.startsWith('http://') || v.startsWith('https://')) {
        continue;
      }
      let filename = v.split('/').pop();
      // Skip if link filename already has extension.
      if (filename.includes('.')) {
        continue;
      }
      elem.attributes.href.value = v + '.html';
    }
  });
})();
