$(function() {
  $(".bin-drop-zone").on("dragenter", function(e) {
    $(this).addClass("hover");
  });

  $(".bin-drop-zone").on("dragleave drop", function(e) {
    $(this).removeClass("hover");
  });
});
