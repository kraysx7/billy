-record(result_packet, {seq_num, field_list, rows, extra}).

-record(ok_packet, {seq_num, affected_rows, insert_id, status, warning_count, msg}).

-record(error_packet, {seq_num, code, msg}).
