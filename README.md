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

#### Build ``leofs_doctor`` from the source code

```shell
$ git clone https://github.com/leo-project/leofs_doctor.git
$ cd leofs_doctor
$ make

## Find a built leofs_doctor under "_red" directory
$ tree -vhD ./_rel/leofs_doctor/bin/
./_rel/leofs_doctor/bin/
├── [1.9K Feb 22 15:06]  leofs_doctor
├── [1.9K Feb 22 15:06]  leofs_doctor-0.2.0
└── [5.2K Feb 22 15:06]  start_clean.boot
```

## How to Deliver

* Archive LeoFS Doctor with the tar command

```shell
$ cd _rel/
$ tar -czf leofs_doctor.tar.gz leofs_doctor
```

* Deliver ``leofs_doctor.tar.gz`` to users


## How to Use
### Prepare it
* Modify **a cookie for distributed Erlang** in [files/vm.args](https://github.com/leo-project/leofs_doctor/blob/develop/files/vm.args) to be able to connect a target node:

```shell
## Cookie for distributed erlang
-setcookie <COOKIE_FOR_DISTRIBUTED_ERLANG>
```

### Execute it
```shell
$ leofs_doctor --help
Usage: leofs_doctor ## related to connection
                  -target_node <TARGET_NODE>
                  ## related to entop
                  [-sort_col <COL_NAME>] [-reverse <yes|no>] [-topn <TOPN>]
                  [-interval <SECS>] [-times <TIMES>]
                  ## related to supervisour tree
                  [-root_sup <SUPERVISOR_NAME>] [-expected_svt <FILENAME>]

## Case-1 - without "expected_svt" option:
$ leofs_doctor -target_node manager_0@127.0.0.1 \
               -sort_col red \
               -reverse y \
               -topn 20 \
               -interval 1 \
               -times 10 \
               -root_sup leo_manager_sup

## Case-2 - with "expected_svt" option:
## (A sample expected supervisor tree: <https://github.com/leo-project/leofs_doctor/blob/develop/files/expected_svt.yml>)
$ leofs_doctor -target_node manager_0@127.0.0.1 \
               -sort_col red \
               -reverse y \
               -topn 20 \
               -interval 1 \
               -times 10 \
               -root_sup leo_manager_sup \
               -expected_svt ~/dev/erlang/leofs_doctor/files/expected_svt.yml
```

### Output

```erlang
- Date: 2016/02/22 17:15:40

- Loaded libraries
            Library Name Version   Desctiption
                    bear 0.8.2     A set of statistics functions for erlang
                 bitcask 1.7.0
                  crypto 3.5       CRYPTO
                eleveldb 2.1.4
                   eunit 2.2.9     EUnit
                  folsom 0.8.2     Erlang based metrics system
                      gs 1.5.16    GS  The Graphics System
                   jiffy 0.14.4    JSON Decoder/Encoder.
                  kernel 3.2       ERTS  CXC 138 10
          leo_backend_db 1.1.15    Leo Backend db
             leo_commons 1.1.6     Leo Commons
              leo_logger 1.2.2     Leo Logger
             leo_manager 1.2.18    LeoFS Manager
                  leo_mq 1.3.18    Leo MQ
      leo_object_storage 1.2.12    Leo Object Storage
                 leo_pod 0.6.6     leo_pod manages worker process pools
   leo_redundant_manager 1.9.22    Leo Redundant Manaeger
                 leo_rpc 0.10.7    leo rpc library
             leo_s3_libs 1.1.8     Leo S3-Libs
          leo_statistics 1.1.10    Leo Statistics lib for Erlang
                     lz4 0.2.2
                  mnesia 4.12.5    MNESIA  CXC 138 12
                observer 2.0.4     OBSERVER version 1
                   recon 2.2.1     Diagnostic tools for production use
           runtime_tools 1.8.16    RUNTIME_TOOLS
                    sasl 2.4.1     SASL  CXC 138 11
         savanna_commons 0.8.16    Savanna's common library
                    snmp 5.1.1     SNMP  CXC 138 13
                  stdlib 2.4       ERTS  CXC 138 10
            syntax_tools 1.6.18    Syntax tools
                   tools 2.7.2     DEVTOOLS  CXC 138 16

- Running apps

                App Name Pid
                  crypto undefined
                  kernel <5504.591.0>
              leo_logger <5504.664.0>
             leo_manager <5504.636.0>
                 leo_rpc <5504.692.0>
          leo_statistics <5504.1395.0>
                  mnesia <5504.752.0>
                    sasl <5504.625.0>
                    snmp <5504.1404.0>
                  stdlib undefined

[entop]
#1:
Node: 'manager_0@127.0.0.1'
 (17/6.4.1.5) unix (darwin 14.5.0) CPU:8 SMP +A:32 +K
Sorting on "Reductions" (Descending), Retrieved in 2ms
Time: local time 13:30:14, up for 000:00:02:42, 0ms latency,
Processes: total 187 (RQ 0) at 2267 RpI using 15153.1k (15163.7k allocated)
Memory: Sys 36775.4k, Atom 707.2k/734.9k, Bin 235.2k, Code 22867.4k, Ets 6320.9k

             Pid Registered Name      Initial Call                   Current Function                         Reductions   Reductions+  MQueue HSize        SSize  HTotal
       <0.688.0> leo_redundant_manage proc_lib:init_p/5              gen_server:loop/6                        798419       798419       0      17731        9      17731
      <0.1157.0> -                    proc_lib:init_p/5              gen_server:loop/6                        556483       556483       0      376          9      376
       <0.691.0> leo_membership_clust proc_lib:init_p/5              gen_server:loop/6                        423614       423614       0      233          9      233
       <0.643.0> -                    proc_lib:init_p/5              gen_server:loop/6                        302428       302428       0      2586         9      2586
       <0.640.0> leo_manager_cluster_ proc_lib:init_p/5              gen_server:loop/6                        116306       116306       0      610          9      610
       <0.689.0> leo_redundant_manage proc_lib:init_p/5              gen_server:loop/6                        108178       108178       0      376          9      376
       <0.771.0> mnesia_tm            proc_lib:init_p/5              mnesia_tm:doit_loop/1                    105689       105689       0      376          17     376
       <0.769.0> mnesia_locker        proc_lib:init_p/5              mnesia_locker:loop/1                     94995        94995        0      233          15     233
         <0.3.0> erl_prim_loader      erlang:apply/2                 erl_prim_loader:loop/3                   81299        81299        0      17731        6      17731
       <0.607.0> code_server          erlang:apply/2                 code_server:loop/1                       77216        77216        0      17731        3      17731
         <0.0.0> init                 otp_ring0:start/2              init:loop/1                              68458        68458        0      2586         2      2586
       <0.588.0> error_logger         proc_lib:init_p/5              gen_event:fetch_msg/5                    59318        59318        0      376          8      376
       <0.606.0> file_server_2        proc_lib:init_p/5              gen_server:loop/6                        57878        57878        0      987          9      987
       <0.644.0> tcp_server_cui_3     proc_lib:init_p/5              prim_inet:accept0/2                      56433        56433        0      10958        15     10958
       <0.785.0> -                    proc_lib:init_p/5              disk_log:loop/1                          46284        46284        0      1598         4      1598
      <0.1156.0> -                    proc_lib:init_p/5              gen_server:loop/6                        43796        43796        0      121536       9      121536
       <0.645.0> tcp_server_cui_2     proc_lib:init_p/5              prim_inet:accept0/2                      43782        43782        0      610          15     610
       <0.646.0> tcp_server_cui_1     proc_lib:init_p/5              prim_inet:accept0/2                      31013        31013        0      10958        15     10958
       <0.669.0> leo_logger_file_e    proc_lib:init_p/5              gen_server:loop/6                        29422        29422        0      2586         9      2586
       <0.690.0> leo_membership_clust proc_lib:init_p/5              gen_server:loop/6                        27807        27807        0      610          9      610

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

## Code
The part of code is derived from [entop](https://github.com/mazenharake/entop).

## License
LeoFS Doctor is distributed under [Apache 2.0 License](http://www.apache.org/licenses/LICENSE-2.0).

## Sponsors
LeoProject/LeoFS is sponsored by [Rakuten, Inc.](http://global.rakuten.com/corp/) and supported by [Rakuten Institute of Technology](http://rit.rakuten.co.jp/).
