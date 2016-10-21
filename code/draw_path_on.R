calc_curvature <- function(v_ego, angle_steers, angle_offset = 0) {
    deg_to_rad = pi/180.
    slip_fator = 0.0014 # slip factor obtained from real data
    steer_ratio = 15.3  # from http://www.edmunds.com/acura/ilx/2016/road-test-specs/
    wheel_base = 2.67   # from http://www.edmunds.com/acura/ilx/2016/sedan/features-specs/
    angle_steers_rad = (angle_steers - angle_offset) * deg_to_rad
    curvature = angle_steers_rad/(steer_ratio * wheel_base * (1 + slip_fator * v_ego^2))
    return(curvature)
}

calc_curvature(100, 30)

calc_lookahead_offset <- function(v_ego, angle_steers, d_lookahead){
    angle_offset=0
    #*** this function returns the lateral offset given the steering angle, speed and the lookahead distance
    curvature = calc_curvature(v_ego, angle_steers, angle_offset)
    # clip is to avoid arcsin NaNs due to too sharp turns
    temp <- d_lookahead * curvature
    temp[temp < -0.999] <- -0.999
    temp[temp > 0.999] <- 0.999
    y_actual = d_lookahead * tan(asin(temp)/2)
    return (y_actual)
}

draw_path_on <- function(speed_ms, angle_steers, color="blue") {
    path_x = seq(0, 100, 5)
    path_y = path_x
    for (i in 1:length(path_x))
        path_y[i] = calc_lookahead_offset(speed_ms, angle_steers, path_x[i])
    points(160 + path_y, 160 - path_x, col = color, pch = 19)
}



