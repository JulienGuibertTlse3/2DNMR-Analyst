# CNN_model.R - CNN Model Architecture and Loading ----
#
# Part of the CNN Peak Detection module for 2D NMR Spectra
#
# Author: Julien Guibert
# Institution: INRAe Toxalim / MetaboHUB


#' Build CNN Peak Predictor Model
#'
#' Constructs a 1D convolutional neural network for peak detection in NMR spectra.
#' The model has two output heads:
#' - Classification head: 3-class softmax (background, peak edge, peak center)
#' - Regression head: 3 values per point (ppm, intensity, FWHH)
#'
#' @return A compiled Keras model ready for training or inference
#'
#' @details
#' Architecture:
#' - Input: 2048 points (1D spectrum slice)
#' - 8 convolutional layers with ReLU activation
#' - Kernel sizes: 11 (feature extraction), 1 (channel mixing), 3 (refinement)
#' - Filter progression: 40 -> 20 -> 10 -> 20 -> 10 -> 30 -> 18 -> 18
#'
#' Loss functions:

#' - Classification: Focal loss (gamma=3.0, alpha=1.5) for class imbalance
#' - Regression: Mean Squared Error (MSE)
#'
#' @examples
#' model <- build_peak_predictor()
#' summary(model)
#'
#' @export
build_peak_predictor <- function() {
  n_points <- 2048
  input <- layer_input(shape = c(n_points, 1), name = "input")
  
  # Feature extraction backbone
  # Uses alternating large kernels (11) for pattern detection
  
  # and 1x1 kernels for channel-wise feature mixing
  x <- input %>%
    layer_conv_1d(filters = 40, kernel_size = 11, padding = "same", activation = "relu") %>%
    layer_conv_1d(filters = 20, kernel_size = 1, padding = "same", activation = "relu") %>%
    layer_conv_1d(filters = 10, kernel_size = 11, padding = "same", activation = "relu") %>%
    layer_conv_1d(filters = 20, kernel_size = 1, padding = "same", activation = "relu") %>%
    layer_conv_1d(filters = 10, kernel_size = 1, padding = "same", activation = "relu") %>%
    layer_conv_1d(filters = 30, kernel_size = 11, padding = "same", activation = "relu") %>%
    layer_conv_1d(filters = 18, kernel_size = 1, padding = "same", activation = "relu") %>%
    layer_conv_1d(filters = 18, kernel_size = 3, padding = "same", activation = "relu")
  
  # Classification head: 3 classes with softmax activation
  # Classes: 0 = background, 1 = peak edge, 2 = peak center
  output_class <- x %>%
    layer_conv_1d(filters = 3, kernel_size = 1, activation = "softmax", name = "class_output")
  
  # Regression head: 3 values per point (ppm position, intensity, FWHH)
  output_reg <- x %>%
    layer_conv_1d(filters = 3, kernel_size = 1, activation = "linear", name = "reg_output")
  
  model <- keras_model(inputs = input, outputs = list(output_class, output_reg))
  
  # Focal loss implementation for handling class imbalance
  # gamma: focusing parameter (higher = more focus on hard examples)
  # alpha: class weight factor
  focal_loss <- function(gamma = 3.0, alpha = 1.5) {
    function(y_true, y_pred) {
      y_true <- k_cast(y_true, "int32")
      
      # Convert y_true to one-hot encoding
      y_true_one_hot <- k_one_hot(y_true, num_classes = 3)
      
      # Clip predictions to avoid log(0)
      epsilon <- k_epsilon()
      y_pred <- k_clip(y_pred, epsilon, 1.0 - epsilon)
      
      # Compute focal loss
      cross_entropy <- -y_true_one_hot * k_log(y_pred)
      weight <- alpha * k_pow(1 - y_pred, gamma)
      loss <- weight * cross_entropy
      
      return(k_sum(loss, axis = -1))
    }
  }
  
  # Compile model with dual loss functions
  model %>% compile(
    loss = list(
      class_output = focal_loss(gamma = 3.0, alpha = 1.5),
      reg_output = "mse"
    ),
    loss_weights = list(
      class_output = 1.0,
      reg_output = 1.0
    ),
    optimizer = optimizer_adam(learning_rate = 1e-4),
    metrics = list(
      class_output = "accuracy"
    )
  )
  
  return(model)
}

# Build and load pre-trained model weights
new_model <- build_peak_predictor()
load_model_weights_tf(new_model, "saved_model/weights")


