[%%cenum
   
type ftype = 
  | SYN
    
  | ACK
  | NACK

  | STREAM
  | RST_STREAM

  | PING
  | PONG


  | GO_AWAY
    
  | SHUTDOWN
  | SHUTDOWN_ACK
 
   


[@@uint8_t]

]





[%%cstruct


type header = {

  cid: uint64_t;
  ftype: uint8_t;
  seq_no: uint32_t;
  
  timestamp: uint64_t;
  len: uint16_t
} [@@big_endian]


]





[%%cstruct


type ack = {
  ack_no: uint32_t;
  rtt: uint64_t;
} [@@big_endian]

]


type stream = {

  sid: Cstruct.uint32;
  fin: bool;
  
  offset: Cstruct.uint64;
  len: Cstruct.uint16;
  data: Cstruct.t
}


[%%cstruct

  type rst_stream = {
    sid: uint32_t; code: uint8_t; offset: uint64_t
  } [@@big_endian]

]




type frame =
  | Syn of int32
  | Ping
  | Pong

  | Ack of Cstruct.t
  | Nack of int32 list

  | Stream of stream
  | Rst_stream of int32
  | Go_away of Cstruct.t

  | Shutdown of int64
  | Shutdown_Ack of int64







module Headers = struct
  type t = {

    cid: int64;
    ftype: int;
    seq_no: int32;

    timestamp: int64;
    len: int
  }


  let write buf t =
    set_header_len buf t.len;
    set_header_cid buf t.cid;
    set_header_ftype buf t.ftype;

    set_header_timestamp buf t.timestamp;
    set_header_seq_no buf t.seq_no;;
    

  let read buf =


    try 
      let cid = get_header_cid buf in
      let len = get_header_ftype buf in
      let seq_no = get_header_seq_no buf in

      let timestamp = get_header_timestamp buf in
      let ftype = get_header_ftype buf in

      Ok {cid; len; seq_no; timestamp; ftype}
    

    with _ ->
      Error "unable to parse headers"


  let size t =
    sizeof_header


  

  
        
  
end




module Frame = struct
  

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




  


  
    
end 
