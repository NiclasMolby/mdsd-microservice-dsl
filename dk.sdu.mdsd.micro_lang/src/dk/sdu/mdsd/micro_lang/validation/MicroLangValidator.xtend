/*
 * generated by Xtext 2.16.0
 */
package dk.sdu.mdsd.micro_lang.validation

import org.eclipse.xtext.validation.Check
import dk.sdu.mdsd.micro_lang.microLang.MicroLangPackage
import dk.sdu.mdsd.micro_lang.microLang.Microservice
import dk.sdu.mdsd.micro_lang.microLang.Endpoint
import dk.sdu.mdsd.micro_lang.MicroLangModelUtil
import com.google.inject.Inject
import dk.sdu.mdsd.micro_lang.microLang.PathPart

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
	
	public static val ENDPOINT_PATH_MULTIPLE_SLASHES = ISSUE_CODE_PREFIX + 'EndpointPathMultipleSlashes'
	
	@Inject
	extension MicroLangModelUtil
	
	@Check
	def checkNoSelfInUses(Microservice microservice) {
		if (microservice.uses.empty) {
			return
		}
		if (microservice.uses.contains(microservice)) {
			error('Microservice "' + microservice.name + '" references itself', 
				MicroLangPackage.eINSTANCE.microservice_Uses, 
				microservice.uses.indexOf(microservice), 
				USES_SELF, 
				microservice.name)
		}
	}
	
	@Check
	def checkMicroserviceNameCapitalSnakeCase(Microservice microservice) {
		if (!microservice.name.equals(microservice.name.toUpperCase)) {
			warning('Microservice name should be written in capitals', 
				MicroLangPackage.eINSTANCE.microservice_Name, 
				INVALID_MICROSERVICE_NAME, 
				microservice.name)
		}
	}
	
	@Check
	def checkEndpointNameLowerFlatCase(Endpoint endpoint) {
		for (PathPart part : endpoint.normalPathParts) {
			val name = part.path
			if (!name.equals(name.toLowerCase)) {
				warning('Endpoint path should be written in lower case', 
					MicroLangPackage.eINSTANCE.endpoint_PathParts, 
					endpoint.pathParts.indexOf(part), 
					INVALID_ENDPOINT_PATH_NAME, 
					name)
			}
		}
	}
	
	@Check
	def checkEndpointOnlyOneSlash(Endpoint endpoint) {
		if (endpoint.path.contains('//')) {
			warning('Endpoint path should not contain consecutive slashes', 
					MicroLangPackage.eINSTANCE.endpoint_PathParts, 
					endpoint.pathParts.indexOf('//'), 
					ENDPOINT_PATH_MULTIPLE_SLASHES, 
					endpoint.path)
		}
	}
	
}
