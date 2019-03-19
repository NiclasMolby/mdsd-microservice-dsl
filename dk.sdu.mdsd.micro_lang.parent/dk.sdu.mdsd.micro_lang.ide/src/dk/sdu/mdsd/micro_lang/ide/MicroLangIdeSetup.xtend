/*
 * generated by Xtext 2.16.0
 */
package dk.sdu.mdsd.micro_lang.ide

import com.google.inject.Guice
import dk.sdu.mdsd.micro_lang.MicroLangRuntimeModule
import dk.sdu.mdsd.micro_lang.MicroLangStandaloneSetup
import org.eclipse.xtext.util.Modules2

/**
 * Initialization support for running Xtext languages as language servers.
 */
class MicroLangIdeSetup extends MicroLangStandaloneSetup {

	override createInjector() {
		Guice.createInjector(Modules2.mixin(new MicroLangRuntimeModule, new MicroLangIdeModule))
	}
	
}