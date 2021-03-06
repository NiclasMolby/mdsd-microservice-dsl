package dk.sdu.mdsd.micro_lang

import static org.eclipse.emf.ecore.util.EcoreUtil.UsageCrossReferencer.find

import dk.sdu.mdsd.micro_lang.microLang.COp
import dk.sdu.mdsd.micro_lang.microLang.Div
import dk.sdu.mdsd.micro_lang.microLang.Element
import dk.sdu.mdsd.micro_lang.microLang.Endpoint
import dk.sdu.mdsd.micro_lang.microLang.Exp
import dk.sdu.mdsd.micro_lang.microLang.Gateway
import dk.sdu.mdsd.micro_lang.microLang.GatewayGivenPath
import dk.sdu.mdsd.micro_lang.microLang.Given
import dk.sdu.mdsd.micro_lang.microLang.Implements
import dk.sdu.mdsd.micro_lang.microLang.Logic
import dk.sdu.mdsd.micro_lang.microLang.LogicAnd
import dk.sdu.mdsd.micro_lang.microLang.Microservice
import dk.sdu.mdsd.micro_lang.microLang.Minus
import dk.sdu.mdsd.micro_lang.microLang.Model
import dk.sdu.mdsd.micro_lang.microLang.Mult
import dk.sdu.mdsd.micro_lang.microLang.NormalPath
import dk.sdu.mdsd.micro_lang.microLang.Number
import dk.sdu.mdsd.micro_lang.microLang.Operation
import dk.sdu.mdsd.micro_lang.microLang.ParameterPath
import dk.sdu.mdsd.micro_lang.microLang.PathPart
import dk.sdu.mdsd.micro_lang.microLang.Plus
import dk.sdu.mdsd.micro_lang.microLang.Return
import dk.sdu.mdsd.micro_lang.microLang.Template
import dk.sdu.mdsd.micro_lang.microLang.TypedParameter
import dk.sdu.mdsd.micro_lang.microLang.Uses
import java.util.ArrayList
import java.util.HashSet
import java.util.List
import java.util.Set
import dk.sdu.mdsd.micro_lang.microLang.Type
import dk.sdu.mdsd.micro_lang.microLang.Method
import dk.sdu.mdsd.micro_lang.microLang.Argument
import dk.sdu.mdsd.micro_lang.microLang.MicroserviceEndpoint

/**
 * Extension utility methods for the various classes of the meta-model.
 */
class MicroLangModelUtil {
	
	def microservices(Model model) {
		model.elements.filter(Microservice)
	}
	
	def templates(Model model) {
		model.elements.filter(Template)
	}
	
	def uses(Element element) {
		element.declarations.filter(Uses).map[target]
	}
	
	def references(Gateway gateway) {
		val microserviceReference = new HashSet<Microservice>()
		gateway.declarations.filter(Endpoint).map[operations].flatten.forEach[op | op.statements.filter(Given).forEach[given | 
			microserviceReference.add(given.left.microservice)
			microserviceReference.add(given.right.microservice)
			if (given.condition.endpoint !== null) {
				microserviceReference.add(given.condition.endpoint.microservice)
			}
		]]
		microserviceReference
	}
	
	def getImplements(Element element) {
		element.declarations.filter(Implements)
	}
	
	def endpoints(Element element) {
		element.declarations.filter(Endpoint)
	}
	
	/**
	 * Provides an Iterable of the endpoints this 'implements' provides, 
	 * consisting of the endpoints declared in its target template, as well as 
	 * any template the target might implement.
	 */
	def Iterable<Endpoint> inheritedEndpoints(Implements implement) {
		implement.target.endpoints + implement.target.implements.flatMap[inheritedEndpoints]
	}
	
	def parameters(Operation operation) {
		operation.statements.filter(TypedParameter)
	}
	
	def parameters(List<GatewayGivenPath> paths) {
		paths.filter[target !== null].map[target]
	}
	
	def parameters(Endpoint endpoint, Operation operation) {
		endpoint.parameterPaths.map[parameter] + operation.parameters
	}
	
	def parameters(List<GatewayGivenPath> paths, Operation operation) {
		paths.filter[target !== null].map[target] + operation.parameters
	}
	
	def returnTypes(Operation operation) {
		operation.statements.filter(Return)
	}
	
	def returnType(Operation operation) {
		operation.returnTypes.head
	}
	
	def hasReturn(Operation operation) {
		operation.returnType !== null
	}
	
	def normalPaths(Endpoint endpoint) {
		endpoint.pathParts.filter(NormalPath)
	}
	
	def parameterPaths(Endpoint endpoint) {
		endpoint.pathParts.filter(ParameterPath)
	}
	
	def mapPaths(List<PathPart> pathParts, (NormalPath) => CharSequence computeNormalPaths, (ParameterPath) => CharSequence computeParameterPaths, String prefixAndJoin) {
		prefixAndJoin + pathParts.map[
			switch it {
				NormalPath: computeNormalPaths.apply(it)
				ParameterPath: computeParameterPaths.apply(it)
			}
		].join(prefixAndJoin)
	}
	
	def path(List<GatewayGivenPath> path) {
		"/"+path.map[name ?: target.name].join("/")
	}
	
	def path(Endpoint endpoint) {
		endpoint.pathParts.mapPaths([name ?: ""], ['{' + parameter.type.name + '}'], '/')
	}
	
	def pathToCompare(List<GatewayGivenPath> path) {
		"/"+path.map[name ?: "{" + target.type.name + "}" ].join("/")
	}
	
	def pathToCompare(Endpoint endpoint) {
		endpoint.pathParts.mapPaths([name ?: ""], ['{' + parameter + '}'], '/')
	}
	
	def containsGiven(Operation operation) {
		!operation.statements.filter(Given).empty
	}
	
	def dispatch List<String> attributes(Logic logic) {
		val attributes = new ArrayList<String>()
		attributes.addAll(logic.left.attributes)
		if(logic.right !== null) {
			attributes.addAll(logic.right.attributes)
		}
		attributes
	}
	
	def dispatch List<String> attributes(LogicAnd logic) {
		val attributesList = new ArrayList<String>()
		attributesList.add(logic.left.left.attribute)
		if(logic.right !== null) {
			attributesList.addAll(logic.right.attributes)
		}
		
		attributesList
	}
	
	def printSet(Set<String> list) {
		val builder = new StringBuilder()
		list.forEach[item, index | 
			if (index > 0) {
				builder.append(", ")
			}
			builder.append(item)
		]
		builder.toString
	}
	
	def operator(COp op) {
		switch op {
			case op.lt: '<'
			case op.gt: '>'
			case op.gte: '>='
			case op.lte: '<='
			case op.eq: '=='
		}
	}
	
	def firstGiven(Operation operation) {
		operation.statements.filter(Given).head
	}
	
	def resolveMethodReference(Operation operation) {
		if (operation.statements.filter(Given).head === null)
			operation
		else  operation.statements.filter(Given).head
	}
	
	def resolveMethodReference(MicroserviceEndpoint microserviceEndpoint, Operation method) {
		resolveMethodReference(microserviceEndpoint, method.method.name)
	}
	
	def resolveMethodReference(MicroserviceEndpoint microserviceEndpoint, String method) {
		val endpoint = microserviceEndpoint.microservice.declarations.filter(Endpoint).findFirst[path == microserviceEndpoint.pathParts.pathToCompare]
		var Operation foundOperation = null
		
		if (endpoint !== null)  {
			foundOperation = endpoint.operations.findFirst[operation | operation.method.name == method]
		}
		
		val inheritedEndpoints = new ArrayList<Endpoint>()
        microserviceEndpoint.microservice.declarations.filter(Implements).forEach[resolve]
        for (Implements implement : microserviceEndpoint.microservice.implements) {
        	val foundEndpoint = implement.inheritedEndpoints.findFirst[path == microserviceEndpoint.pathParts.pathToCompare]
        	if (foundEndpoint !== null) inheritedEndpoints.add(foundEndpoint)
        }
        
        if(!inheritedEndpoints.empty) {
        	for(Endpoint inheritedEndpoint : inheritedEndpoints) {
        		foundOperation = inheritedEndpoint.operations.findFirst[operation |
        			operation.method.name == method
        		] ?: foundOperation
        	}
        }
        foundOperation
	}
	
	def resolve(Exp exp) {
		exp.comp
	}
	
	def dispatch int comp(Plus plus) {
		plus.left.comp + plus.right.comp
	}
	
	def dispatch int comp(Minus minus) {
		minus.left.comp - minus.right.comp
	}
	
	def dispatch int comp(Mult mult) {
		mult.left.comp * mult.right.comp
	}
	
	def dispatch int comp(Div div) {
		div.left.comp / div.right.comp
	}
	
	def dispatch int comp(Number num) {
		num.value
	}
	
	def attribute(LogicAnd logic) {
		logic.left.left.attribute
	}
	
	def exp(LogicAnd logic) {
		logic.left.right
	}
	
	def void resolve(Implements implement) {
		val args = implement.arguments.map[name]
		implement.target.parameters.forEach [ parameter, index |
			find(parameter, parameter.eContainer).forEach[EObject.resolve(args.get(index))]
		]
		implement.target.implements.forEach[resolve]
	}

	def dispatch resolve(Argument argument, String arg) {
		argument.name = arg
	}

	def dispatch resolve(NormalPath path, String arg) {
		path.name = arg
	}

	def dispatch resolve(Method method, String arg) {
		method.name = arg
	}

	def dispatch resolve(TypedParameter parameter, String arg) {
		parameter.name = arg
	}

	def dispatch resolve(Type type, String arg) {
		type.name = arg
	}
}