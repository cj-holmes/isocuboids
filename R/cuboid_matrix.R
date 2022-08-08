#' Produce pseudo 3-D images from matrices
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Convert a numeric matrix into cuboids that are raised from the ground plane and viewed isometrically (by default)
#'
#' @details
#' Shading of the cuboid faces is done naively - it's a simple reduction in brightness, so there is no clever physics going on here!
#' If angles a1 and a2 are changed from their default values, the view will no longer by isometric.
#'
#' @param mat a matrix
#' @param cuboid_fill a vector of colours to be mapped to cuboid height (for images this defaults to the original pixel colour)
#' @param cuboid_col a colour for the cuboid stroke outline
#' @param orientation orientation of the output (1, 2, 3 or 4) (default = 1)
#' @param a1 angle of rotation around the vertical axis in degrees (default = 45) (default a1 and a2 give isometric view)
#' @param a2 angle of rotation around the horizontal axis in degrees (default = asin(tan(pi/6))*(180/pi) ~35.264) (default a1 and a2 give isometric view)
#' @param shading a vector of 3 numbers between 0 and 1. The amount of shading applied to each cuboid face. Order is top, left, right. (Default = c(0, 0.25, 0.5))
#' @param return_data return dataframe instead of image (default = FALSE)
#' @param show_axes show axes on the output image? (default = TRUE)
#' @param axis_cols a vector of colours for the three axes
#' @param show_height_plane show a polygon plane at the max height of the image? (default = FALSE)
#' @param height_plane_fill colour for the height plane fill
#' @param height_plane_col colour for the height plane stroke outline
#' @param height_plane_lty line type for the height plane stroke outline
#' @param height_plane_alpha alpha value applied to the fill colour of the height plane
#' @param verbose print messages during processing
#'
#' @examples
#' cuboid_matrix(matrix(runif(100), nrow = 10), cuboid_col = 1)
#'
#' @export
cuboid_matrix <-
    function(
        mat,
        cuboid_fill = NULL,
        cuboid_col = NA,
        orientation = 1,
        a1 = 45,
        a2 = asin(tan(pi/6))*(180/pi),
        shading = c(0, 0.25, 0.5),
        return_data = FALSE,
        show_axes = TRUE,
        axis_cols = c("orange", "blue", "red"),
        show_height_plane = FALSE,
        height_plane_fill = NA,
        height_plane_col = axis_cols[3],
        height_plane_lty = "longdash",
        height_plane_alpha = 0.1,
        verbose = FALSE){

        # Checks and tests
        if(!("matrix" %in% class(mat))) stop("mat must be a matrix")
        if(!(a1 >= 0 & a1 <= 90 & a2 >= 0 & a2 <= 90)) stop("Both angles a1 and a2 must be between 0 and 90")
        if(any(is.na(mat))) stop("Missing values not allowed in matrix")
        if(any(mat < 0)) stop("Negative values not allowed in matrix")
        if(!all(c(shading >= 0, shading <= 1))) stop("All shading values must be between 0 and 1")

        if(verbose) message("Rotating and processing matrix")

        # Assign matrix to i
        i <- mat

        # Rotate/transpose matrix for desired orientation
        if(orientation == 1){
            i <- i}
        else if(orientation == 2){
            i <- i[nrow(i):1, ,drop = F] |> t()}
        else if(orientation == 3){
            i <- i[nrow(i):1, ncol(i):1, drop = F]}
        else if(orientation == 4){
            i <- i[,ncol(i):1,  drop = F] |> t()}
        else{
            stop("orientation must be one of 1, 2, 3 or 4")}

        # Store matrix dimensions for computation of plot axes at the end
        x_dim <- ncol(i)
        z_dim <- nrow(i)
        y_dim <- max(i)

        # Create dataframe of x,y,z coordinates
        # Do not scale the height - plot height as raw matrix values
        d <-
            expand.grid(z = (nrow(i)-1):0, x = 0:(ncol(i)-1)) |>
            dplyr::mutate(y = as.vector(i), cuboid_id = dplyr::row_number())

        # If no fill colour is provided for a matrix, make it viridis Mako
        # (Trim off the first 10 dark colours)
        if(is.null(cuboid_fill)) cuboid_fill <- hcl.colors(100, palette = "mako")[11:100]

        # Append unit cube, project and visualise ----------------------------------
        if(verbose) message("Projecting coordinates")

        d <-
            d |>
            dplyr::arrange(dplyr::desc(x), dplyr::desc(z)) |>
            dplyr::mutate(colorRamp(cuboid_fill)(scales::rescale(y)) |> t() |> rgb2hsv() |> t() |> tibble::as_tibble()) |>
            tidyr::expand_grid(unit_cube_df) |>
            dplyr::mutate(
                ux = ux+x,
                uy = uy*y,
                uz = uz+z,
                project(x=ux, y=uy, z=uz, va=a1, ha=a2) |>
                    dplyr::as_tibble(.name_repair = ~c("px", "py", "pz")),
                v_adjusted = dplyr::case_when(face == "top" ~ v*(1-shading[1]),
                                              face == "left" ~ v*(1-shading[2]),
                                              face == "right" ~ v*(1-shading[3])),
                col_adjusted = hsv(h, s, v_adjusted),
                plot_group = paste0(face, cuboid_id)) |>
            dplyr::mutate(plot_group = factor(plot_group, levels = unique(plot_group)))

        # Return dataframe here
        if(return_data) return(dplyr::as_tibble(d))

        # Plot ---------------------------------------------------------------------
        if(verbose) message("Rendering plot output")
        ggplot2::ggplot()+
            ggplot2::annotate(
                geom="polygon",
                x = d$px,
                y = d$py,
                fill = d$col_adjusted,
                group = d$plot_group,
                col = cuboid_col)+
            ggplot2::coord_equal(expand = TRUE)+
            # Add axes
            {if(show_axes)
                add_axes(
                    a1 = a1,
                    a2 = a2,
                    x_dim = x_dim,
                    z_dim = z_dim,
                    y_dim = y_dim,
                    x_axis_col = axis_cols[1],
                    z_axis_col = axis_cols[2],
                    y_axis_col = axis_cols[3],
                    data = d)}+
            # Add height plane
            {if(show_height_plane)
                add_height_plane(
                    a1 = a1,
                    a2 = a2,
                    x_dim = x_dim,
                    z_dim = z_dim,
                    y_dim = y_dim,
                    col = height_plane_col,
                    lty = height_plane_lty,
                    fill = height_plane_fill,
                    alpha = height_plane_alpha)}+
            NULL

    }

