


module Header: sig

  type t =  {

    cid: int64;
    ftype: int;
    seq_no: int64;

    timestamp: int64;
    len: int
  }



  val write: Cstruct.t -> t -> unit
  val read: Cstruct.t -> (t, string) result
  val size: t -> int

  
end 









module Payload: sig
  
   type t =
    | SYN of {seq_no: int64; window: int64}
    | ACK of {seq_no: int64; rtt: int64}

    | NACK of (int64 list)
    | STREAM of {
      sid: int32; fin: bool;
      offset: int64; len: int;
      payload: Cstruct.t 
    }

    | RST_STREAM of {sid: int32; code: int; off: int64}
    | PING
    | PONG

    | GO_AWAY of int64

    | SHUTDOWN
    | SHUTDOWN_ACK





  val write: Cstruct.t -> t -> unit
  val read: Header.t -> Cstruct.t -> (t, string) result

  val size: t -> int
  
end 






