## About The Project

The Detailed Site Assessment (DSA) was initiated in coordination with the Camp Coordination and Camp Management (CCCM) Cluster in order to provide the humanitarian community with up-to-date information on the location of IDP sites, the conditions and capacity of the sites, and an estimate of the severity of humanitarian needs of residents.

## Using this Repo

This repo contains the scripts necessary to clean the raw data for the DSA. The inputs required are:

1. The survey response data, which is accessed via an API call to the Kobo server
2. The survey questions and choices
3. The CCCM site level data
4. The field officer reference data, detailing which region and district the FOs are responsible for
 
The code has several outputs:

1. The cleaning logs
2. The combined cleaning logs, for the field dashboard
3. The survey response data, for the dashboard
4. The geospatial checks
5. The survey referrals
6. The KI data

## Dependencies

Beyond the required inputs, the script requires you to set up `keyring` and download two packages from GitHub `ImpactFunctions`, which has been developed by Alex Stephenson to create re-usable, modularised code, and `Robotoolbox`, for querying the kobo server via API. `Keyring` is used for securely calling passwords within scripts. You set a password using `keyring::key_set("Test_Service", "My_Username")`, which will prompt an entry box for your password. Enter the password and then when you call `keyring::key_get("Test_Service", "My_Username")` it will return your password. This can be done in the command line. 

## Contact

Please contact alex.stephenson@impact-initiatives.org for any queries. Or fork the repo and make a pull request. 