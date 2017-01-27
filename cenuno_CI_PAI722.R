# 
# Author: Cristian Nuno
# Purpose: Demonstrate the Confidence Interval of Slopes
# Date: January 26, 2017
#

# Confidence Interval of Classroom Size on Test Scores
# Source of Data: PAI 722 Homework 2 Linear Regression Output

points( x = c(-0.38393:-0.37608), y = c(0,0), 
        pch = 19, col = "firebrick", 
        type = "l", lwd = 5)  # model 2
points( x = c( -0.47108, 0.20708 ), y = c(0, 0), 
        pch = 19, col = "firebrick", 
        type = "l", lwd = 5)  # model 4

# Classroom size slope
class.size <- c(-0.378, -0.380, -0.132)
ci.left <- c( -0.41328, -0.38393, -0.47108 )
ci.right <- c( -0.34272, -0.37608, 0.20708 )

# Create plot layout
slope <- range( -0.5:0.5 )     # x-axis
n.trials <- range( -0.5:0.5 )  # y-axis

par( mfrow = c( 3, 1) )
# Plot Model 1
plot( slope, n.trials,
      main = "Model 1: 95% Confidence Interval\nClass Size Impact on Test Scores",
      ylab = "# of Trials",
      yaxt = "n",
      xaxt = "n",
      xlab = "",
      type = "n" )

# Label Y axis
mtext( "Infinity", side = 2, line = 1, adj = 1  )
mtext( "0",        side = 2, line = 1, adj = .01)

# Add Estimated Slope (Beta 1) value for each model
abline( v = class.size[ 1 ], lwd = 2, col = "dodgerblue4" ) # model 1 slope (solid)
abline( v = 0, lwd = 2, lty = 2, col = "black" )   # No impact (dashed)

# Add 95% Confidence Interval
points( x = c(ci.left[1], ci.right[1]), y = c(0,0), 
        pch = 19, col = "firebrick", 
        type = "b", lwd = 2) # model 1

# Annonate Data Points
text( x = c(ci.left[1], ci.right[1]), y = c(0,0),
      labels = c(-0.41, -0.34), 
      pos = c(2, 4), cex = 0.6, col = "firebrick" )

# Add Legend
legend( x = 0.2, y = 0.5,
        cex = 1,
        legend = c("No Impact", "Slope", "Confidence\nInterval"),
        bty = "n", lty = c(2, 1, 4), 
        lwd = c(1, 1, 1), 
        col = c("black", "dodgerblue4", "firebrick")
        )

# Plot Model 2
plot( slope, n.trials,
      main = "Model 2: 95% Confidence Interval\nClass Size Impact on Test Scores",
      ylab = "# of Trials",
      yaxt = "n",
      xaxt = "n",
      xlab = "",
      type = "n" )

# Label Y axis
mtext( "Infinity", side = 2, line = 1, adj = 1  )
mtext( "0",        side = 2, line = 1, adj = .01)

# Add Estimated Slope (Beta 1) value for each model
abline( v = class.size[ 2 ], lwd = 2, col = "dodgerblue4" ) # model 1 slope (solid)
abline( v = 0, lwd = 2, lty = 2, col = "black" )   # No impact (dashed)

# Add 95% Confidence Interval
points( x = c(ci.left[2], ci.right[2]), y = c(0,0), 
        pch = 19, col = "firebrick", 
        type = "b", lwd = 2) # model 1

# Annonate Data Points
text( x = c(ci.left[2], ci.right[2]), y = c(0,0),
      labels = c(-0.38, -0.37), 
      pos = c(2, 4), cex = 0.6, col = "firebrick" )

# Add Legend
legend( x = 0.2, y = 0.5,
        cex = 1,
        legend = c("No Impact", "Slope", "Confidence\nInterval"),
        bty = "n", lty = c(2, 1, 4), 
        lwd = c(1, 1, 1), 
        col = c("black", "dodgerblue4", "firebrick")
)

# Plot Model 4
plot( slope, n.trials,
      main = "Model 4: 95% Confidence Interval\nClass Size Impact on Test Scores",
      xlab = "Slope (Beta 1)",
      ylab = "# of Trials",
      yaxt = "n",
      type = "n" )

# Label Y axis
mtext( "Infinity", side = 2, line = 1, adj = 1  )
mtext( "0",        side = 2, line = 1, adj = .01)

# Add Estimated Slope (Beta 1) value for each model
abline( v = class.size[ 3 ], lwd = 2, col = "dodgerblue4" ) # model 1 slope (solid)
abline( v = 0, lwd = 2, lty = 2, col = "black" )   # No impact (dashed)

# Add 95% Confidence Interval
points( x = c(ci.left[3], ci.right[3]), y = c(0,0), 
        pch = 19, col = "firebrick", 
        type = "b", lwd = 2) # model 1

# Annonate Data Points
text( x = c(ci.left[3], ci.right[3]), y = c(0,0),
      labels = c(-0.47, 0.20), 
      pos = c(2, 4), cex = 0.6, col = "firebrick" )

# Add Legend
legend( x = 0.2, y = 0.5,
        cex = 1,
        legend = c("No Impact", "Slope", "Confidence\nInterval"),
        bty = "n", lty = c(2, 1, 4), 
        lwd = c(1, 1, 1), 
        col = c("black", "dodgerblue4", "firebrick")
)