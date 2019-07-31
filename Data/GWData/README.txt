D06-KING raw groundwater pressure transducer data compiled from the remote computer
Date: 2017-08-09
Contact: Guy Litt (glitt@battelleecology.org) 720.921.2662 (compiled sensor data)
Nora Catolico (ncatolico@battelleecology.org) (survey data management)

Approximate sensor data range: 2016-08-31 to 2017-08-08

Site notes: A flood on March 29, 2017 damaged much of the sensors at KING. The survey
data for groundwater well elevation was conducted pre-storm in the summer of 2016.

Included contents:
Data_File*.gz: raw data files for each individual groundwater sensor
-Raw Data - Stream 000 reports vented groundwater pressure data in kPa
-EPROMID is a unique ID for each sensor (AquaTROLL 200 in groundwater wells)
-The EPROMID can be matched to groundwater wells using EPROMID_Mapping.txt
The groundwater sensor's elevation may be determined using the following:
1. OffsetFromGround_m column in EPROMID_Mapping.txt
2. Height column in D06_KING_AIS_Survey.csv
The offset values in EPROMID_Mapping.txt must be subtracted from the Height
value for the OW_[groundwater well number]_BOT, where BOT represents the bottom
of the well external well casing (aka ground level).