PCAplotter is an application for Principal Component Analysis (PCA).
PCAplotter - a free, simple-to-use online application to perform PCA, visualize the results, and produce ready-for-publication images for experiments with one-three factors design. PCAplotter is written in R language and converted into the web-based application using Shiny-package. It doesn't require installation, and can be easily accessed online.

To start working with PCAplotter access it online on https://scienceinside.shinyapps.io/PCAplotter/. To use the application, follow the instructions below:
1. Click 1st Browse button to locate and upload DATA file in a predefined view. The data organization rules can be found on the Tutorial page of the application or learned by downloading provided exemplary files.
2. The PCA is based on variance. This makes it sensitive to the variables that level is strongly different from the majority of the other variables. To avoid an effect caused by this, it is usually recommended to transform or scale the dataset. The application provides options to perform PCA on the raw data, on Log-transformed (use DATA Log-transformation switch) and on scaled (the data is standardized to mean 0 and standard deviation 1) data (use DATA scaling switch).
3. Most data do not require visualization of three, and even two factors. In respect to this, use a proper NUMBER OF FACTORS selection. At least one factor (1: Color) is required for graphical visualization of PCA plot. 
4. After determining the needed number of factors, consequently click 2nd, 3rd, 4th Browse buttons (from the top), locate and upload COLOR, SHAPE, SIZE file(s) in the predefined view (can be learned from downloading exemplary files too.
5. Click on Data tab to see the uploaded data frame, factor strings, and presence of missing values (optionally).
6. After uploading the files click on PCA tabs to obtain graphical results.
7. PCA color theme slider provides options for visualization based on color preferences, including an option suggested for color-blinded users by Wong, and greyscale option for users interested in black-white figures only. The data that consist of more than 4 groups can be difficult to distinguish in this mode.
8. Use PCA plot style switches to add sample labels, and/or 95% CI group ellipse (based on COLOR factor).
9. PCA summary tab provides information about the input of every object to the samples separation on PCA plot.
10. The graphical and numeric outputs on PCA and PCA summary tabs can be downloaded to a user’s PC.
