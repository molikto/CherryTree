package utils

import com.mohiva.play.silhouette.api.util.HTTPLayer
import com.mohiva.play.silhouette.impl.services.{GravatarService, GravatarServiceSettings}
import javax.inject.Inject

import scala.concurrent.Future
import scala.util.Random

class ProfileImageService @Inject() (httpLayer: HTTPLayer, settings: GravatarServiceSettings = GravatarServiceSettings()) extends GravatarService(httpLayer, settings) {


  private val animals = Seq("meerkat","aardvark","addax","alligator","alpaca","anteater","antelope","aoudad","ape","argali","armadillo","baboon","badger","basilisk","bat","bear","beaver","bighorn","bison","boar","budgerigar","buffalo","bull","bunny","burro","camel","canary","capybara","cat","chameleon","chamois","cheetah","chimpanzee","chinchilla","chipmunk","civet","coati","colt","cougar","cow","coyote","crocodile","crow","deer","dingo","doe","dung beetle","dog","donkey","dormouse","dromedary","duckbill platypus","dugong","eland","elephant","elk","ermine","ewe","fawn","ferret","finch","fish","fox","frog","gazelle","gemsbok","gila monster","giraffe","gnu","goat","gopher","gorilla","grizzly bear","ground hog","guanaco","guinea pig","hamster","hare","hartebeest","hedgehog","highland cow","hippopotamus","hog","horse","hyena","ibex","iguana","impala","jackal","jaguar","jerboa","kangaroo","kitten","koala","lamb","lemur","leopard","lion","lizard","llama","lovebird","lynx","mandrill","mare","marmoset","marten","mink","mole","mongoose","monkey","moose","mountain goat","mouse","mule","musk deer","musk-ox","muskrat","mustang","mynah bird","newt","ocelot","okapi","opossum","orangutan","oryx","otter","ox","panda","panther","parakeet","parrot","peccary","pig","octopus","thorny devil","starfish","blue crab","snowy owl","chicken","rooster","bumble bee","eagle owl","polar bear","pony","porcupine","porpoise","prairie dog","pronghorn","puma","puppy","quagga","rabbit","raccoon","ram","rat","reindeer","rhinoceros","salamander","seal","sheep","shrew","silver fox","skunk","sloth","snake","springbok","squirrel","stallion","steer","tapir","tiger","toad","turtle","vicuna","walrus","warthog","waterbuck","weasel","whale","wildcat","bald eagle","wolf","wolverine","wombat","woodchuck","yak","zebra","zebu")

  def randomImage: String = s"https://www.randomlists.com/img/animals/${animals(Random.nextInt(animals.size))}.jpg"

  override def retrieveURL(email: String): Future[Option[String]] = super.retrieveURL(email).map(_.orElse(Some(randomImage)))
}
