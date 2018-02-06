#!/bin/bash

echo "Starting service"

. set-environment

export GATEWAY_IP=$(ip route | grep default | cut -d ' ' -f 3)
export STATSD_HOST=${STATSD_HOST:-$GATEWAY_IP}

/usr/local/bin/set-environment

set -x
caching-socket-proxy \
  ${AWS_REGION+                       --region                            "${AWS_REGION}"                       } \
  ${LISTENING_PORT+                   --listening-port                    "${LISTENING_PORT}"                   } \
  ${REMOTE_HOST+                      --remote-host                       "${REMOTE_HOST}"                      } \
  ${REMOTE_PORT+                      --remote-port                       "${REMOTE_PORT}"                      } \
  ${CACHE_TTL+                        --cache-ttl                         "${CACHE_TTL}"                        } \
  ${QUERY_TIMEOUT+                    --query-timeout                     "${QUERY_TIMEOUT}"                    } \
  ${STATSD_HOST+                      --statsd-host                       "${STATSD_HOST}"                      } \
  ${STATSD_SAMPLE_RATE+               --statsd-sample-rate                "${STATSD_SAMPLE_RATE}"               } \
  ${STATSD_TAGS+                      --statsd-tags                       "${STATSD_TAGS}"                      } \
  ${LOG_LEVEL+                        --log-level                         "${LOG_LEVEL}"                        }
