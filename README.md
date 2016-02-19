# LeoFS Doctor

## Purpose
Quick investigation into OTP applications on remote nodes with less operations.

## Requirements
- Standalone
- No need to install Erlang on a node executing this tool
- No need to install some special erlang modules on remote nodes
- Display information entop can retrieve
- Dispaly information related to Mnesia
- Display information related to supervisor tree

## Design
### Interfaces
Build as a CUI tool.

- Input

```shell
### how to use
$ leofs_doctor --help
Usage: leofs_doctor ## related to connection
                  [-target_node <TARGET_NODE>]
                  ## related to entop
                  [-sort_col <COL_NAME>] [-reverse <yes|no>] [-topn <TOPN>]
                  ## related to supervisour tree
                  [-root_sup <SUPERVISOR_NAME>] [-expected_svt <FILENAME>]

### sample
$ leofs_doctor -target_node manager_0@127.0.0.1 -sort_col red -reverse y -topn 20 -root_sup leo_manager_sup -expected_svt ~/dev/erlang/leofs_doctor/files/expected_svt.yml
```

```yaml
## file format used by leofs_doctor's param (-expected_svt)
## Simply write supervisour trees with YAML like this
- tcp_server_sup:
  - tcp_server_cui_3
  - tcp_server_cui_2
  - tcp_server_cui_1
  - tcp_server_json_16
  - tcp_server_json_15
  - tcp_server_json_14
  - tcp_server_json_13
  - tcp_server_json_12
  - tcp_server_json_11
  - tcp_server_json_10
  - tcp_server_json_9
  - tcp_server_json_8
  - tcp_server_json_7
  - tcp_server_json_6
  - tcp_server_json_5
  - tcp_server_json_4
  - tcp_server_json_3
  - tcp_server_json_2
  - tcp_server_json_1
- leo_manager_cluster_monitor
- leo_manager_table_sync
- leo_manager_ring_sync
- leo_mq_sup:
  - leo_backend_db_sup:
    - mq_fail_rebalance_message_2
    - mq_fail_rebalance_message_1
    - mq_fail_rebalance_message_0
    - mq_monitor_node_message
  - mq_fail_rebalance
  - mq_fail_rebalance_consumer3
  - mq_fail_rebalance_consumer2
  - mq_fail_rebalance_consumer1
  - mq_monitor_node
  - mq_monitor_node_consumer1
- leo_redundant_manager_sup:
  - leo_redundant_manager_worker
  - leo_redundant_manager
  - leo_membership_cluster_remote
  - leo_membership_cluster_local
  - leo_mdcr_tbl_sync
```

- Output

```erlang
Date: 2016/02/19 11:39:09

[entop]
Node: 'manager_0@127.0.0.1' 
 (17/6.4) unix (darwin 15.3.0) CPU:4 SMP +A:32 +K
Sorting on "HSize" (Descending), Retrieved in 3ms
Time: local time 11:39:09, up for 002:21:24:44, 1ms latency, 
Processes: total 189 (RQ 0) at 49682 RpI using 14940.6k (14953.8k allocated)
Memory: Sys 37381.4k, Atom 768.5k/774.9k, Bin 59.8k, Code 22852.0k, Ets 8231.8k

             Pid Registered Name      Initial Call                   Current Function                         Reductions   MQueue HSize        SSize  HTotal       
      <0.1146.0> -                    proc_lib:init_p/5              gen_server:loop/6                        144007       0      121536       9      121536       
       <0.593.0> rex                  proc_lib:init_p/5              gen_server:loop/6                        267918       0      28690        9      28690        
         <0.3.0> erl_prim_loader      erlang:apply/2                 erl_prim_loader:loop/3                   114396       0      17731        6      17731        
       <0.606.0> code_server          erlang:apply/2                 code_server:loop/1                       8386558      0      17731        3      17731        
      <0.1147.0> -                    proc_lib:init_p/5              gen_server:loop/6                        159510330    0      17731        9      17731        
       <0.631.0> release_handler      proc_lib:init_p/5              gen_server:loop/6                        75273        0      10958        9      10958        
       <0.638.0> tcp_server_sup       proc_lib:init_p/5              gen_server:loop/6                        66907        0      4185         9      4185         
       <0.588.0> application_controll erlang:apply/2                 gen_server:loop/6                        68784        0      2586         7      2586         
       <0.697.0> leo_rpc_sup          proc_lib:init_p/5              gen_server:loop/6                        60331        0      2586         9      2586         
       <0.779.0> -                    proc_lib:init_p/5              disk_log:loop/1                          140856       0      2586         4      2586         
    <0.31259.12> -                    erlang:apply/2                 shell:get_command1/5                     14609        0      2586         16     2586         
       <0.663.0> -                    proc_lib:init_p/5              application_master:main_loop/2           56137        0      1598         6      1598         
       <0.670.0> leo_mq_sup           proc_lib:init_p/5              gen_server:loop/6                        70811        0      1598         9      1598         
       <0.673.0> mq_fail_rebalance_me proc_lib:init_p/5              gen_server:loop/6                        255121       0      1598         9      1598         
       <0.680.0> mq_fail_rebalance_me proc_lib:init_p/5              gen_server:loop/6                        250290       0      1598         9      1598         
       <0.681.0> mq_fail_rebalance_me proc_lib:init_p/5              gen_server:loop/6                        248238       0      1598         9      1598         
       <0.692.0> mq_monitor_node_mess proc_lib:init_p/5              gen_server:loop/6                        85845        0      1598         9      1598         
       <0.743.0> disk_log_server      proc_lib:init_p/5              gen_server:loop/6                        73777        0      1598         9      1598         
      <0.1124.0> snmpa_supervisor     proc_lib:init_p/5              gen_server:loop/6                        93614        0      1598         9      1598         
         <0.0.0> init                 otp_ring0:start/2              init:loop/1                              173667       0      987          2      987          

[mnesia]
- System Info
	- version: "4.12.5"
	- is_running: yes
	- lock_queue: []
	- transactions: []
	- transaction_failures: 6
	- transaction_commits: 228511
	- transaction_restarts: 3
	- transaction_log_writes: 268

- Table Info

               Table Name Access Mode  Memory     Size       
                   schema read_write   4114       23         
        leo_storage_nodes read_write   305        0          
        leo_gateway_nodes read_write   305        0          
       leo_rebalance_info read_write   305        0          
            leo_histories read_write   305        0          
   leo_available_commands read_write   4260       43         
      leo_cluster_manager read_write   305        0          
          leo_members_cur read_write   305        0          
         leo_members_prev read_write   305        0          
       leo_s3_credentials read_write   321        1          
         leo_s3_endpoints read_write   329        2          
   leo_s3_user_credential read_write   321        1          
          leo_system_conf read_write   322        1          
         leo_cluster_info read_write   321        1          
         leo_cluster_stat read_write   305        0          
       leo_cluster_member read_write   305        0          
             leo_ring_cur read_write   95         0          
             leo_ring_prv read_write   95         0          
           leo_s3_buckets read_write   305        0          
             leo_s3_users read_write   323        1          
               sv_schemas read_write   317        1          
            sv_metric_grp read_write   365        2          
               sv_columns read_write   561        7          

[supervisor tree]
- current: [[{"tcp_server_sup",
              ["tcp_server_cui_3","tcp_server_cui_2","tcp_server_cui_1",
               "tcp_server_json_16","tcp_server_json_15","tcp_server_json_14",
               "tcp_server_json_13","tcp_server_json_12","tcp_server_json_11",
               "tcp_server_json_10","tcp_server_json_9","tcp_server_json_8",
               "tcp_server_json_7","tcp_server_json_6","tcp_server_json_5",
               "tcp_server_json_4","tcp_server_json_3","tcp_server_json_2",
               "tcp_server_json_1"]}],
            "leo_manager_cluster_monitor","leo_manager_table_sync",
            "leo_manager_ring_sync",
            [{"leo_mq_sup",
              [[{"leo_backend_db_sup",
                 ["mq_fail_rebalance_message_2","mq_fail_rebalance_message_1",
                  "mq_fail_rebalance_message_0","mq_monitor_node_message"]}],
               "mq_fail_rebalance","mq_fail_rebalance_consumer3",
               "mq_fail_rebalance_consumer2","mq_fail_rebalance_consumer1",
               "mq_monitor_node","mq_monitor_node_consumer1"]}],
            [{"leo_redundant_manager_sup",
              ["leo_redundant_manager_worker","leo_redundant_manager",
               "leo_membership_cluster_remote","leo_membership_cluster_local",
               "leo_mdcr_tbl_sync"]}]]
- expected: [[{"tcp_server_sup",
               ["tcp_server_cui_3","tcp_server_cui_2","tcp_server_cui_1",
                "tcp_server_json_16","tcp_server_json_15",
                "tcp_server_json_14","tcp_server_json_13",
                "tcp_server_json_12","tcp_server_json_11",
                "tcp_server_json_10","tcp_server_json_9","tcp_server_json_8",
                "tcp_server_json_7","tcp_server_json_6","tcp_server_json_5",
                "tcp_server_json_4","tcp_server_json_3","tcp_server_json_2",
                "tcp_server_json_1"]}],
             "leo_manager_cluster_monitor","leo_manager_table_sync",
             "leo_manager_ring_sync",
             [{"leo_mq_sup",
               [[{"leo_backend_db_sup",
                  ["mq_fail_rebalance_message_2",
                   "mq_fail_rebalance_message_1",
                   "mq_fail_rebalance_message_0","mq_monitor_node_message"]}],
                "mq_fail_rebalance","mq_fail_rebalance_consumer3",
                "mq_fail_rebalance_consumer2","mq_fail_rebalance_consumer1",
                "mq_monitor_node","mq_monitor_node_consumer1"]}],
             [{"leo_redundant_manager_sup",
               ["leo_redundant_manager_worker","leo_redundant_manager",
                "leo_membership_cluster_remote",
                "leo_membership_cluster_local","leo_mdcr_tbl_sync"]}]]
- missing: []
```

### How it works
- Basics
  - Fork entop in order to reduce development cost by reusing (initialize|load local BEAM binaries on remote nodes|connect remote nodes|handling STDOUT) functionalities

- related to entop
https://github.com/mazenharake/entop
  - Simply use entop WITHOUT functionalities which enable users to control sort order and so on via STDIN

- related to mnesia
http://erlang.org/documentation/doc-6.4/lib/mnesia-4.12.5/doc/html/mnesia.html
  - Simply use `mnesia:system_info/1` and `mnesia:table_info/2`

- related to supervisour tree
http://erlang.org/documentation/doc-6.4/lib/stdlib-2.4/doc/html/supervisor.html
  - Simply compare the current supervisour trees on memory with the expected one represented by a YAML file

## Sponsors

LeoProject/LeoFS is sponsored by [Rakuten, Inc.](http://global.rakuten.com/corp/) and supported by [Rakuten Institute of Technology](http://rit.rakuten.co.jp/).

## Code
The part of code is derived from [entop](https://github.com/mazenharake/entop).
