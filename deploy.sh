#!/usr/bin/env bash

FN_BUCKET="***REMOVED***" aws cloudformation package \
  --template-file=infrastructure/cf.yaml \
  --s3-bucket "$FN_BUCKET" > \
  deployment_stack.yaml

aws cloudformation deploy \
  --stack-name "noobs-notification" \
  --region us-west-2 \
  --capabilities CAPABILITY_IAM \
  --template-file deployment_stack.yaml
