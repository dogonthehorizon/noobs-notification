#!/usr/bin/env bash

FN_BUCKET="***REMOVED***" 
OUT_FILE="deploy.yaml"

aws cloudformation package \
  --template-file=infrastructure/template.yaml \
  --s3-bucket "$FN_BUCKET" \
  --output-template-file "$OUT_FILE"

aws cloudformation deploy \
  --stack-name "noobs-notification" \
  --region us-west-2 \
  --capabilities CAPABILITY_IAM \
  --template-file "$OUT_FILE"
