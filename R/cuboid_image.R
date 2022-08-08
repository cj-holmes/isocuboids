#' Produce pseudo 3-D images from pixel values of 2-D images
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Convert image pixels into cuboids that are raised from the ground plane and viewed isometrically (by default)
#'
#' @details
#' By default cuboids are raised in proportion to pixel brightness on a scale between 1 and 10.
#' If angles a1 and a2 are changed from their default values, the view will no longer by isometric.
#' Shading of the cuboid faces is also done naively - it's a simple reduction in brightness, so there is no clever physics going on here!
#' Some further reading on the Mitchell-Netravali resize filter https://www.cs.utexas.edu/~fussell/courses/cs384g-fall2013/lectures/mitchell/Mitchell.pdf and here https://legacy.imagemagick.org/Usage/filter/#mitchell
#'
#' @param img a magick image or image file path/URL
#' @param res \code{img} is resized to be \code{res} pixels wide. A value of NULL keeps original size. (default = 60)
#' @param resize_filter resize filter to be used \code{magick::filter_types} (default = 'Mitchell')
#' @param height_scale a two value vector defining the lower and upper heights for scaling (default = c(1, 10))
#' @param height_map an expression containing r, g, b, h, s, v, x, or y (default = v)
#' @param cuboid_fill a vector of colours to be mapped to cuboid height (defaults to the original pixel colour)
#' @param cuboid_col a colour for the cuboid stroke outline
#' @param orientation orientation of the image prior to being projected (1, 2, 3 or 4) (default = 1)
#' @param crop_square crop image to a square (default = TRUE)
#' @param crop_gravity gravity of square crop if \code{crop_square = TRUE} (value from \code{magick::gravity_types()})
#' @param a1 angle of rotation around the vertical axis in degrees (default = 45) (default a1 and a2 give isometric view)
#' @param a2 angle of rotation around the horizontal axis in degrees (default = asin(tan(pi/6))*(180/pi) ~35.264) (default a1 and a2 give isometric view)
#' @param shading a vector of 3 numbers between 0 and 1. The amount of shading applied to each cuboid face. Order is top, left, right. (Default = c(0, 0.25, 0.5))
#' @param return_data return dataframe instead of image (default = FALSE)
#' @param show_axes show axes on the output image? (default = TRUE)
#' @param axis_cols a vector of colours for the three axes
#' @param show_height_plane show a polygon plane at the max height of the image (default = FALSE)
#' @param height_plane_fill colour for the height plane fill
#' @param height_plane_col colour for the height plane stroke outline
#' @param height_plane_lty line type for the height plane stroke outline
#' @param height_plane_alpha alpha value applied to the fill colour of the height plane
#' @param verbose print messages during processing
#'
#' @examples
#' cuboid_image(
#'     img = 'https://www.r-project.org/Rlogo.png',
#'     crop_square = FALSE,
#'     height_map = h)
#' @export
cuboid_image <-
    function(
        img,
        res = 60,
        resize_filter = "Mitchell",
        height_scale = NULL,
        height_map = v,
        cuboid_fill = NULL,
        cuboid_col = NA,
        orientation = 1,
        crop_square = TRUE,
        crop_gravity = "center",
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

        # Check that the image is magick or a character string
        if(!(class(img) %in% c("magick-image", "character"))) stop("img must be a a {magick} image or character path to an image file")
        if(!(a1 >= 0 & a1 <= 90 & a2 >= 0 & a2 <= 90)) stop("Both angles a1 and a2 must be between 0 and 90")
        if(!(is.null(height_scale) | all(height_scale >= 0))) stop("Both height scale values must be greater than or equal to 0")
        if(!all(c(shading >= 0, shading <= 1))) stop("All shading values must be between 0 and 1")


        if(verbose) message("Resizing, rotating and processing image")

        # If img is already a magick image, leave it as is - otherwise read it in
        if(class(img) == "magick-image"){i <- img} else {i <- magick::image_read(img)}

        # Store original image width and height
        original_image_info <- magick::image_info(i)
        original_width <- original_image_info$width
        original_height <- original_image_info$height

        # Crop the image to a square or keep original aspect ratio and scale to specified horizontal res
        if(is.null(res)){
            res <- original_width
        } else if(crop_square){

            # Check resolution and warn if larger than original
            if(res > original_width) warning("res is larger than original image width")
            if(res > original_height) warning("res is larger than original image height")

            i <-
                i |>
                magick::image_resize(geometry = paste0(res,"x",res,"^"), filter = resize_filter) |>
                magick::image_crop(geometry = paste0(res,"x",res), gravity = crop_gravity)

        } else {

            # Check resolution and warn if larger than original
            if(res > original_width) warning("res is larger than original image width")
            i <- i |> magick::image_resize(geometry = paste0(res,"x"), filter = resize_filter)
        }

        # Create automatic height scale if none provided
        if(is.null(height_scale)) height_scale <- c(1,  10)

        # Rotate/flip/flop image according to orientation specified
        if(orientation == 1){
            i <- magick::image_flip(i)}
        else if(orientation == 2){
            i <- magick::image_rotate(i, 90) |> magick::image_flip()}
        else if(orientation == 3){
            i <- magick::image_flop(i)}
        else if(orientation == 4){
            i <- magick::image_rotate(i, 90) |> magick::image_flop()}
        else{
            stop("orientation must be one of 1, 2, 3 or 4")}

        # Compute new image dimensions after any rotation
        # Store image dimensions for computation of plot axes at the end
        x_dim <- magick::image_info(i)$width
        z_dim <- magick::image_info(i)$height
        y_dim <- max(height_scale)

        # Quote the expression that will be used to compute the height map
        expr <- dplyr::enquo(height_map)

        # Wrangle image to dataframe and scale y (height)
        d <-
            i |>
            magick::image_raster() |>
            # Set pixel coordinates to start at (0,0)
            dplyr::mutate(x = x - 1, y = y - 1) |>
            dplyr::rename(z = y) |>
            dplyr::mutate(
                col2rgb(col) |>  t() |> dplyr::as_tibble() |> dplyr::rename(r = red, g = green, b = blue),
                col2rgb(col) |> rgb2hsv() |> t() |> dplyr::as_tibble(),
                cuboid_id = dplyr::row_number(),
                y := !!expr,
                y = scales::rescale(y, to = height_scale))

        # If cuboid_fill is not NULL Set the colours for the output here
        # This will replace h,s,and v from the original pixel colours
        if(!is.null(cuboid_fill)){
            d <-
                d |>
                dplyr::mutate(colorRamp(cuboid_fill)(scales::rescale(y)) |>
                                  t() |>
                                  rgb2hsv() |>
                                  t() |>
                                  tibble::as_tibble())}

        # Append unit cube, project and visualise ----------------------------------
        if(verbose) message("Projecting coordinates")
        d <-
            d |>
            dplyr::arrange(dplyr::desc(x), dplyr::desc(z)) |>
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
        # Compute axes and height plane
        if(verbose) message("Rendering plot output")
        ggplot2::ggplot()+
            ggplot2::annotate(geom="polygon",
                              x = d$px,
                              y = d$py,
                              fill = d$col_adjusted,
                              group = d$plot_group,
                              col = cuboid_col)+
            ggplot2::coord_equal(expand = TRUE)+
            # Add axes
            {if(show_axes)
                add_axes(a1 = a1,
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
                add_height_plane(a1 = a1,
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
