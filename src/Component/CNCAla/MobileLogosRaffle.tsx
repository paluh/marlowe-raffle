import React from "react";
import { Box, Center, Image, Link, SimpleGrid } from "@chakra-ui/react";

export function _MobileLogosRaffle() {
	return (
		<>
			<Link href="https://climateneutralcardano.org/">
				<Image
					src={"/assets/images/cnc_logo_dark.png"}
					alt={"CNC"}
					loading="lazy"
					objectFit="contain"
					w={"72"}
				/>
			</Link>
			<SimpleGrid
				columns={{ base: 1, md: 3 }}
				gap={{ base: "5", md: "6" }}
			>
				<Center>
					<Link href="https://cardanoproxies.io/">
						<Image
							src={"/assets/images/Proxies_logo.png"}
							alt={"CNC"}
							loading="lazy"
							objectFit="contain"
							w={"36"}
						/>
					</Link>
				</Center>
				<Center>
					<Link href="https://empowa.io/">
						<Image
							src={"/assets/images/Empowa_logo.svg"}
							alt={"Empowa"}
							loading="lazy"
							objectFit="contain"
							w={"32"}
						/>
					</Link>
				</Center>
				<Center>
					<Link href="https://orderofthekraken.io/">
						<Image
							src={"/assets/images/OrderOfTheKraken_logo.png"}
							alt={"Anvil"}
							loading="lazy"
							objectFit="contain"
							w={"20"}
						/>
					</Link>
				</Center>
				<Center>
					<Link href="https://newm.io/">
						<Image
							src={"/assets/images/NEWM_logo.png"}
							alt={"CNC"}
							loading="lazy"
							objectFit="contain"
							w={"16"}
						/>
					</Link>
				</Center>
				<Center>
					<Link href="https://www.chainsofwar.io/">
						<Image
							src={"/assets/images/ChainsOfWar_logo.png"}
							alt={"CNC"}
							loading="lazy"
							objectFit="contain"
							w={"16"}
						/>
					</Link>
				</Center>
				<Center>
					<Link href="https://metera.io/">
						<Image
							src={"/assets/images/Metera_logo.png"}
							alt={"CNC"}
							loading="lazy"
							objectFit="contain"
							w={"32"}
						/>
					</Link>
				</Center>
				<Center>
					<Link href="https://www.gerowallet.io/">
						<Image
							src={"/assets/images/GeroWallet_logo.svg"}
							alt={"CNC"}
							loading="lazy"
							objectFit="contain"
							w={"36"}
						/>
					</Link>
				</Center>
				<Box></Box>
			</SimpleGrid>
		</>
	);
}
