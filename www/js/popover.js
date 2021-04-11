removePopoverOnClick = function() {
  $(document).on("click", function(e) {
    if ($(e.target).attr("data-toggle") !== "popover" && $(e.target).parents('[data-toggle="popover"]').length === 0) {
      $('[data-toggle="popover"]').popover("hide");
    }
  });
};

$(removePopoverOnClick);
