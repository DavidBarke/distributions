$(function() {
  $(".bin-drop-zone").on("dragenter", function(e) {
    $(this).parents(".main-footer").addClass("dragged");
  });

  $(".bin-drop-zone").on("dragleave drop", function(e) {
    $(this).parents(".main-footer").removeClass("dragged");
  });
});
