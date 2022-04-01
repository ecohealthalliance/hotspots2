To download the original raw data from AWS S3, run the following command. (You currently need to have access to EHA's AWS infrastructure for this to work.)

```
cd data-raw
aws s3 sync s3://hotspots2/data-raw .
```

You can also get the full `data-raw` directory as ZIP file attached to the `data-raw` release of this repository.
