import arcpy
arcpy.ImportToolbox(r"@\Spatial Statistics Tools.tbx")
arcpy.stats.HotSpots(
    Input_Feature_Class=r"Incidence_06\Incidence months\Basis_Inzidenz_totalpop_month_ALL_06",
    Input_Field="I_1920_05",
    Output_Feature_Class=r"W:\Desktop\SPARK\SPARK_05\SPARK.gdb\HotSpot_month_192005_5NN",
    Conceptualization_of_Spatial_Relationships="K_NEAREST_NEIGHBORS",
    Distance_Method="EUCLIDEAN_DISTANCE",
    Standardization="ROW",
    Distance_Band_or_Threshold_Distance=None,
    Self_Potential_Field=None,
    Weights_Matrix_File=None,
    Apply_False_Discovery_Rate__FDR__Correction="NO_FDR",
    number_of_neighbors=5
)
