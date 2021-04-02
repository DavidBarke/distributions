Shiny.addCustomMessageHandler("distributions.update-sortable", function(msg) {
  Shiny.setInputValue(
      msg.inputId,
      $.map($(msg.selector)[0].children, function(child) {
        return $(child).attr("data-rank-id");
      })
    );
});
