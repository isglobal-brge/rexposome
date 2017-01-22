setMethod(
    f = "plotIntegration",
    signature = "ResultSet",
    definition = function(object, cmpX=1, cmpY=2, tcolors, lb.th=0.20, legend.show=TRUE,
                          ...) {
        if(missing(tcolors)) {
            tcolors <- rainbow(length(featureData(object)))
            names(tcolors) <- names(object)
        }

        if(object@class_origin == "<m:mcca>") {
            .plot_integration_mcca(object, tcolors=tcolors, lb.th=lb.th,
                                   legend.show=legend.show, ...)
        } else if(object@class_origin == "<m:mcia>") {
            .plot_integration_mcia(object, cmpX=cmpX, cmpY=cmpY, ...)
        }
})
