# leofs_diag

## Purpose
Quick investigation into OTP applications on remote nodes with less operations.

## Requirements
- Standalone
- No need to install Erlang on a node executing this tool
- No need to install some special erlang modules on remote nodes
- Display information entop|recon can do (recon will cover entop can't)
- Dispaly information related to Mnesia
- Display information related to supervisor tree

## Design
### Interfaces
Build as a CUI tool.

- Input

```shell
$ leofs-diag --help
Usage: leofs-diag ## related to connection
                  [-name <NAME>|-sname <SNAME>] [-setcookie <COOKIE>]
                  ## related to entop
                  [-sort_col <COL_NAME>] [-reverse <yes|no>] [-n <TOPN>]
                  ## related to recon
                  [-bin_leak <TOPN>] [-proc_count <ATTR_NAME,TOPN>] [-inet_count] 
                  ## related to mnesia
                  [-system_info] [-table_info <TABLES>]
                  ## related to supervisour tree
                  [-expected_svt <FILENAME>]
       <TARGETNODE>
```

```yaml
## file format used by leofs-diag's param (-expected_svt)
## Simply write supervisour trees with YAML like this
- leo_storage_sup
  - leo_redundant_manager_sup
    - leo_mdcr_tbl_sync
    - leo_membership_cluster_local
    - leo_mq_sup
      - leo_delete_dir_queue_consumer_1
      - leo_delete_dir_queue
      - leo_comp_meta_with_dc_queue_consumer_1
      - leo_comp_meta_with_dc_queue
      - leo_sync_obj_with_dc_queue_consumer_1
      - leo_sync_obj_with_dc_queue
      - leo_recovery_node_queue_consumer_1
      - leo_recovery_node_queue
      - leo_async_deletion_queue_consumer_1
      - leo_async_deletion_queue
      - leo_rebalance_queue_consumer_1
      - leo_rebalance_queue
      - leo_sync_by_vnode_id_queue_consumer_1
      - leo_sync_by_vnode_id_queue
      - leo_per_object_queue_consumer_1
      - leo_per_object_queue
      - mq_persistent_node_consumer_1
      - mq_persistent_node
    - leo_redundant_manager
    - leo_redundant_manager_worker
  - leo_object_storage_sup
    - leo_object_storage_event_notifier
    - leo_compact_fsm_controller
    - leo_object_storage_0
    - leo_compact_worker_0
    - leo_backend_db_sup
      - leo_delete_dir_queue_message_0
      - leo_comp_meta_with_dc_queue_message_0
      - leo_metadata_0
  - leo_storage_msg_collector
```

- Output

```shell
## related to entop
Same to entop with arguments specified via CUI (sort columns, reverse, topN) 

## related to recon
Same to recon with arguments specified via CUI (bin_leak, proc_count, inet_count...)

## related to mnesia
Display information retrieved by mnesia:system_info/1 and mnesia:table_info/2

[Mnesia]
- System info
  - is_running: yes
  - local_tables: [leo_s3_bucket, leo_s3_endpoint, leo_s3_user, ...]
  - lock_queue: []
  - transactions: []
  - transaction_failures: 18
  - transaction_commits: 192
  - transaction_restarts: 9
  - transaction_log_writes: 223
  - version: 4.12.5

- Table info
  - leo_s3_bucket
    - memory: 16384
    - size: 120
    - version: 1
  - leo_s3_endpoint
    - memory: 8192
    - size: 3
    - version: 1

## related to supervisour tree
Output unmatch(unseen) processes with its supervisours like below.

[Supervisour Tree]

The below processes are not found.
leo_storage_sup
  |- leo_object_storage_sup
       |- leo_object_storage_13
       |- leo_compact_worker_13
  |- leo_redundant_manager_sup
       |- leo_mq_sup
            |- leo_delete_dir_queue_consumer_8
```

### How it works
- Basics
  - Fork entop in order to reduce development cost by reusing (initialize|load local BEAM binaries on remote nodes|connect remote nodes|handling STDOUT) functionalities

- related to entop
https://github.com/mazenharake/entop
  - Simply use entop WITHOUT functionalities which enable users to control sort order and so on via STDIN
  
- related to recon
https://github.com/ferd/recon
  - Simply use recon functions which can retrieve information can NOT be with entop such as `recon:bin_leak`, `recon:inet_count` etc...

- related to mnesia
http://erlang.org/documentation/doc-6.4/lib/mnesia-4.12.5/doc/html/mnesia.html
  - Simply use `mnesia:system_info/1` and `mnesia:table_info/2`

- related to supervisour tree
http://erlang.org/documentation/doc-6.4/lib/stdlib-2.4/doc/html/supervisor.html
  - Simply compare the current supervisour trees on memory with the expected one represented by a YAML file

## Sponsors

LeoProject/LeoFS is sponsored by [Rakuten, Inc.](http://global.rakuten.com/corp/) and supported by [Rakuten Institute of Technology](http://rit.rakuten.co.jp/).
