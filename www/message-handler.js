// This recieves messages of type "testmessage" from the server.
Shiny.addCustomMessageHandler("cookie_message",
                              function(message) {
                                alert(document.cookie);
                              }
);


Shiny.addCustomMessageHandler("cookie_write",
                              function(message) {
                                document.cookie = "jfztf = 5326";
                              }
);


Shiny.addCustomMessageHandler("cookie_out",
                              function(message) {
                                var number = document.cookie;
                                Shiny.onInputChange("mydata_2", number);
                              }
);