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
