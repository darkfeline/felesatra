(function(){
  // Only run if this is on IPFS.
  if (!window.location.href.includes("ipfs")) {
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
