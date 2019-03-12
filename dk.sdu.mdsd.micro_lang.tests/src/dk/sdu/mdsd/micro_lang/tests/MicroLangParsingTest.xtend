/*
 * generated by Xtext 2.16.0
 */
package dk.sdu.mdsd.micro_lang.tests

import com.google.inject.Inject
import dk.sdu.mdsd.micro_lang.MicroLangModelUtil
import dk.sdu.mdsd.micro_lang.microLang.Model
import org.eclipse.xtext.testing.InjectWith
import org.eclipse.xtext.testing.extensions.InjectionExtension
import org.eclipse.xtext.testing.util.ParseHelper
import org.eclipse.xtext.testing.validation.ValidationTestHelper
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.^extension.ExtendWith

import static org.junit.Assert.assertNull
import static org.junit.Assert.assertTrue

import static extension org.junit.Assert.assertEquals
import static extension org.junit.Assert.assertSame

@ExtendWith(InjectionExtension)
@InjectWith(MicroLangInjectorProvider)
class MicroLangParsingTest {
	
	@Inject
	extension ParseHelper<Model>
	
	@Inject
	extension ValidationTestHelper
	
	@Inject
	extension MicroLangModelUtil
	
	@Test
	def testMicroserviceNoEndpoints() {
		val model = '''
			microservice TEST_SERVICE @ localhost:5000 {
			}
		'''.parse
		model.assertNoErrors
		val microservice = model.microservices.head
		
		'TEST_SERVICE'.assertEquals(microservice.name)
		'localhost'.assertEquals(microservice.location.location)
		5000.assertEquals(microservice.location.port)
		assertTrue(microservice.endpoints.empty)
	}
	
	@Test
	def testEndpointNoParametersOrReturn() {
		val model = '''
			microservice TEST_SERVICE @ localhost:5000 {
				GET /login
			}
		'''.parse
		model.assertNoErrors
		val microservice = model.microservices.head
		
		1.assertEquals(microservice.endpoints.size)
		
		val endpoint = microservice.endpoints.head
		
		'GET'.assertEquals(endpoint.method)
		'/login'.assertEquals(endpoint.pathPartsString.head)
		assertTrue(endpoint.parameters.empty)
		assertNull(endpoint.returnType)
	}
	
	@Test
	def testEndpointNoPath() {
		val model = '''
			microservice TEST_SERVICE @ localhost:5000 {
				GET /
			}
		'''.parse
		model.assertNoErrors
		val microservice = model.microservices.head
		val endpoint = microservice.endpoints.head
		
		'GET'.assertEquals(endpoint.method)
		'/'.assertEquals(endpoint.pathPartsString.head)
		assertTrue(endpoint.parameters.empty)
		assertNull(endpoint.returnType)
	}
	
	@Test
	def testMicroserviceMultipleEndpoints() {
		val model = '''
			microservice TEST_SERVICE @ localhost:5000 {
				GET /login
				POST /
				DELETE /user
			}
		'''.parse
		model.assertNoErrors
		val microservice = model.microservices.head
		
		3.assertEquals(microservice.endpoints.size)
		
		val endpoints = microservice.endpoints
		
		'GET'.assertEquals(endpoints.get(0).method)
		'/login'.assertEquals(endpoints.get(0).pathPartsString.head)
		'POST'.assertEquals(endpoints.get(1).method)
		'/'.assertEquals(endpoints.get(1).pathPartsString.head)
		'DELETE'.assertEquals(endpoints.get(2).method)
		'/user'.assertEquals(endpoints.get(2).pathPartsString.head)
	}
	
	@Test
	def testEndpointPathParameter() {
		val model = '''
			microservice TEST_SERVICE @ localhost:5000 {
				GET /login/{userId}
			}
		'''.parse
		model.assertNoErrors
		val microservice = model.microservices.head
		val endpoint = microservice.endpoints.head
		
		2.assertEquals(endpoint.pathParts.size)
		
		'GET'.assertEquals(endpoint.method)
		'/login'.assertEquals(endpoint.pathPartsString.head)
		'/{userId}'.assertEquals(endpoint.pathPartsString.last)
	}
	
	@Test
	def testEndpointParametersNoReturn() {
		val model = '''
			microservice TEST_SERVICE @ localhost:5000 {
				GET /login {
					string username
					char[] password
				}
			}
		'''.parse
		model.assertNoErrors
		val microservice = model.microservices.head
		val endpoint = microservice.endpoints.head
		
		2.assertEquals(endpoint.parameters.size)
		
		'string'.assertEquals(endpoint.parameters.head.type)
		'username'.assertEquals(endpoint.parameters.head.name)
		'char[]'.assertEquals(endpoint.parameters.last.type)
		'password'.assertEquals(endpoint.parameters.last.name)
		assertNull(endpoint.returnType)
	}
	
	@Test
	def testEndpointReturnTypeNoParameters() {
		val model = '''
			microservice TEST_SERVICE @ localhost:5000 {
				GET /login {
					return bool
				}
			}
		'''.parse
		model.assertNoErrors
		val microservice = model.microservices.head
		val endpoint = microservice.endpoints.head
		
		assertTrue(endpoint.parameters.empty)
		'bool'.assertEquals(endpoint.returnType.type)
	}
	
	@Test
	def testEndpointParametersAndReturnType() {
		val model = '''
			microservice TEST_SERVICE @ localhost:5000 {
				GET /average {
					int[] numbers
					
					return double
				}
			}
		'''.parse
		model.assertNoErrors
		val microservice = model.microservices.head
		val endpoint = microservice.endpoints.head
		
		1.assertEquals(endpoint.parameters.size)
		
		'int[]'.assertEquals(endpoint.parameters.head.type)
		'numbers'.assertEquals(endpoint.parameters.head.name)
		'double'.assertEquals(endpoint.returnType.type)
	}
	
	@Test
	def testMultipleMicroservicesMultipleEndpoints() {
		val model = '''
			microservice TEST_SERVICE @ localhost:5000 {
				GET /average {
					int[] numbers
					
					return double
				}
				POST /{id}/data {
					string id
				}
			}
			microservice SECOND_SERVICE @ localhost:5001 {
				GET /login
				POST / {
					return bool
				}
				DELETE /user
			}
			microservice MOVIE_SERVICE @ localhost:5002 {
				PUT /movies {
					string name
					string description
				}
				DELETE /movies/{id} {
					return bool
				}
			}
		'''.parse
		model.assertNoErrors
		3.assertEquals(model.microservices.size)
		val firstMicroservice = model.microservices.get(0)
		val secondMicroservice = model.microservices.get(1)
		val thirdMicroservice = model.microservices.get(2)
		
		2.assertEquals(firstMicroservice.endpoints.size)
		3.assertEquals(secondMicroservice.endpoints.size)
		2.assertEquals(thirdMicroservice.endpoints.size)
	}
	
	@Test
	def testUsesOtherMicroservice() {
		val model = '''
			microservice TEST_SERVICE @ localhost:5000 {
			}
			microservice SECOND_SERVICE @ localhost:5001 {
			}
			microservice MOVIE_SERVICE @ localhost:5002 {
				uses TEST_SERVICE
				uses SECOND_SERVICE
			}
		'''.parse
		model.assertNoErrors
		val microservices = model.microservices
		val uses = microservices.last.uses
		2.assertEquals(uses.size)
		microservices.get(0).assertSame(uses.get(0))
		microservices.get(1).assertSame(uses.get(1))
	}
	
}