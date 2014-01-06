open Eliom_content
open Printf

module F = struct

  let page ~title contents =
    (Eliom_tools.F.html ~title ~css:[["css"; "inhca.css"]]
      (Html5.F.body (Html5.F.h1 [Html5.F.pcdata title] :: contents)))

  let send_error ~code msg =
    let hdr = sprintf "Error %d" code in
    Eliom_registration.Html5.send ~code
      (Eliom_tools.F.html ~title:hdr ~css:[["css"; "inhca.css"]]
	Html5.F.(body [h1 [pcdata hdr]; p [pcdata msg]]))

end

let get_request_headers () =
  let ri = Eliom_request_info.get_ri () in
  let frame = ri.Ocsigen_extensions.ri_http_frame in
  let frame_header = frame.Ocsigen_http_frame.frame_header in
  let hdrs = Ocsigen_http_frame.Http_header.get_headers frame_header in
  let open Http_headers in
  let ht = NameHtbl.create 13 in
  Http_headers.iter (NameHtbl.add ht) hdrs;
  ht

let http_error code msg =
  Lwt.fail (Ocsigen_http_frame.Http_error.Http_exception (code, Some msg, None))
let http_error_f code fmt = ksprintf (http_error code) fmt

let authorize_admin () =
  match Inhca_config.auth_http_header_cp#get with
  | None -> Lwt.return_unit
  | Some h ->
    let hdrs = get_request_headers () in
    lwt user =
      try Lwt.return (Http_headers.NameHtbl.find hdrs (Http_headers.name h))
      with Not_found -> http_error 500 "Missing authentication header."
    in
    if List.mem user Inhca_config.auth_admins_cp#get
    then Lwt.return_unit
    else http_error 403 "Admin access required."
