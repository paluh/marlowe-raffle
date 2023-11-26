import { Image } from "@chakra-ui/react";

export function _Logo({}) {
	return (
		<Image
			src={"/assets/images/cnc_logo_dark.png"}
			alt={"CNC"}
			loading="lazy"
			objectFit="contain"
			w={"40"}
		/>
	);
}
