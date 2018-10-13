


module Headers: sig

  type t = {

    connection_id: int64;
    ftype: int;
    seq_no: int32;

    timestamp: int64;
    len: int
  }


  val write: Cstruct.t -> t -> unit
  val read: Cstruct.t -> ('a, string) result
  val size: t -> int

  
end 









module Frame: sig
  
  type t =
    | SYN of {seq_no: int64; window: int64}
    | ACK of {ack_no: int64; rtt: int64}
             
    | NACK of (int64 list)
    | STREAM of stream

    | RST_STREAM of rst_stream
    | PING
    | PONG

    | GO_AWAY of int64 
    | SHUTDOWN of int64

    | SHUTDOWN_ACK of int64

  and stream = {
      stream_id: int32; fin: bool;
      offset: int64; len: int;
      data: Cstruct.t 
    }

  and rst_stream = {sid: int32; code: int; off: int32};;



  val write: Cstruct.t -> t -> unit
  val read: Cstruct.t -> (t, string) result

  val size: t -> int
  
end 






