jQuery.noConflict();
jQuery(function() {
  jQuery(".refbody").hide();
  jQuery(".refnum").click(function(event) {
    jQuery(this.nextSibling).toggle();
    event.stopPropagation();
  });
  jQuery("body").click(function(event) {
    jQuery('.refbody').hide();
  });
});
