import React from "react";
import { Center, Image, Link, SimpleGrid, Stack } from "@chakra-ui/react";

export function _DesktopLogos() {
	return (
		<Stack align="center">
			<Link href="https://climateneutralcardano.org/">
				<Image
					src={"/assets/images/cnc_logo_dark.png"}
					alt={"CNC"}
					loading="lazy"
					objectFit="contain"
					w={"xs"}
				/>
			</Link>
			<SimpleGrid columns={{ base: 7 }} gap={{ base: "2" }}>
				<Center>
					<Link href="https://cardanoproxies.io/">
						<Image
							src={"/assets/images/Proxies_logo.png"}
							alt={"Proxies"}
							loading="lazy"
							objectFit="contain"
							w={"32"}
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
							w={"28"}
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
					<Link href="https://metera.io/">
						<Image
							src={"/assets/images/Metera_logo.png"}
							alt={"CNC"}
							loading="lazy"
							objectFit="contain"
							w={"28"}
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
					<Link href="https://www.gerowallet.io/">
						<Image
							src={"/assets/images/GeroWallet_logo.svg"}
							alt={"CNC"}
							loading="lazy"
							objectFit="contain"
							w={"32"}
						/>
					</Link>
				</Center>
			</SimpleGrid>
		</Stack>
	);
}
