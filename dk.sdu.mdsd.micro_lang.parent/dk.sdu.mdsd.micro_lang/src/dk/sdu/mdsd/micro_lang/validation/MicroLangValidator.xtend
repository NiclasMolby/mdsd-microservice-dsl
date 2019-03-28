/*
 * generated by Xtext 2.16.0
 */
package dk.sdu.mdsd.micro_lang.validation

import com.google.inject.Inject
import dk.sdu.mdsd.micro_lang.MicroLangModelUtil
import dk.sdu.mdsd.micro_lang.microLang.MicroLangPackage
import dk.sdu.mdsd.micro_lang.microLang.Microservice
import dk.sdu.mdsd.micro_lang.microLang.NormalPath
import dk.sdu.mdsd.micro_lang.microLang.Uses
import org.eclipse.xtext.validation.Check

/**
 * This class contains custom validation rules. 
 *
 * See https://www.eclipse.org/Xtext/documentation/303_runtime_concepts.html#validation
 */
class MicroLangValidator extends AbstractMicroLangValidator {
	
	protected static val ISSUE_CODE_PREFIX = 'dk.sdu.mdsd.micro_lang.'
	
	public static val USES_SELF = ISSUE_CODE_PREFIX + 'UsesSelf'
	
	public static val INVALID_MICROSERVICE_NAME = ISSUE_CODE_PREFIX + 'InvalidMicroserviceName'
	
	public static val INVALID_ENDPOINT_PATH_NAME = ISSUE_CODE_PREFIX + 'InvalidEndpointPathName'
	
	@Inject
	extension MicroLangModelUtil
	
	@Check
	def checkSelfNotInUses(Uses uses) {
		val container = uses.eContainer as Microservice
		if (uses.target === container) {
			error('Microservice "' + container.name + '" references itself', 
				MicroLangPackage.eINSTANCE.uses_Target, 
				USES_SELF, 
				uses.target.name)
		}
	}
	
	@Check
	def checkMicroserviceNameIsUpperCase(Microservice microservice) {
		if (!microservice.name.equals(microservice.name.toUpperCase)) {
			warning('Microservice name should be written in upper case', 
				MicroLangPackage.eINSTANCE.element_Name, 
				INVALID_MICROSERVICE_NAME, 
				microservice.name)
		}
	}
	
	@Check
	def checkNormalPathIsLowerCase(NormalPath path) {
		if (path.name === null) {
			return
		}
		val name = path.name
		if(!name.equals(name.toLowerCase)) {
			warning('Endpoint path should be written in lower case', 
					MicroLangPackage.eINSTANCE.normalPath_Name, 
					INVALID_ENDPOINT_PATH_NAME, 
					name)
		}
	}
	
}
