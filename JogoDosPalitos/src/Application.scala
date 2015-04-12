
import scala.util.Random
import scala.io.StdIn.readLine

case class Jogador(palitos: Int, ID: String)

object Application {

	def main(args: Array[String]) {
		// SEMPRE sou o jogador número 1
		val numeroDeJogadores = lerNumeroDeJogadores
		// Começo a primeira rodada
		começarRodada(criarJogadores(3, numeroDeJogadores), decidirQuemJogaPrimeiro(numeroDeJogadores), 1)
	}

	// Começo uma nova rodada do jogo
	def começarRodada(jogadores: List[Jogador], primeiroJogador: Int, numero: Int): Unit = {
		// Quando jogo primeiro não preciso esperar as informações sobre as apostas dos outros jogadores
		def jogarPrimeiro(): Unit = {
			val minhaJogada = jogadaRandomica(jogadores.head.palitos)
			val minhaAposta = decidirAposta(jogadores, List(), minhaJogada)
			perguntarAteSim("Nesta rodada eu sou o primeiro a jogar, todos já fizeram suas jogadas? Posso começar a rodada? (sim/não) R: ")
			perguntarAteSim("Ja posso mostrar minha aposta? (sim/não) R:")
			dizer("Minha aposta é: " + minhaAposta + "\n")
			perguntarAteSim("Posso mostrar minha jogada? (sim/não) R: ")
			dizer("Minha jogada é: " + minhaJogada + "\n")
		}

		// Se eu não for o primeiro a jogar, primeiro preciso das informações sobre as apostas dos outros jogadores
		// pra então fazer a minha aposta
		def jogar(): Unit = {
			perguntarAteSim("Todos já fizeram suas jogadas? Posso começar a rodada? (sim/não) R: ")
			val apostas = lerApostasDeCadaJogador(jogadores, primeiroJogador)
			val minhaJogada = jogadaRandomica(jogadores.head.palitos)
			val minhaAposta = decidirAposta(jogadores, apostas, minhaJogada)
			perguntarAteSim("Ja posso mostrar minha aposta? (sim/não) R:")
			dizer("Minha aposta é: " + minhaAposta + "\n")
			perguntarAteSim("Posso mostrar minha jogada? (sim/não) R: ")
			dizer("Minha jogada é: " + minhaJogada + "\n")
		}

		jogadores.size match {
			case 1 =>
				println("O jogador " + jogadores.head.ID + " ficou em último lugar, :( \n")
				println("      #################")
				println("      ## FIM DE JOGO ##")
				println("      #################\n")
			case _ =>
				val numeroDeJogadores = jogadores.size
				println("*******************************")
				println("   Começando a rodada " + numero)
				println("*******************************\n")
				println("Status dos jogadores:")
				jogadores.foreach(jogador => println("Jogador " + jogador.ID + " => " + jogador.palitos + " palitos"))
				println("\nPrimeiro a jogar => Jogador " + primeiroJogador + "\n")
				primeiroJogador match {
					case 1 =>
						jogarPrimeiro()
					case _ =>
						jogar()
				}
				// Depois de jogar, pergunto se alguem acertou o numero total de palitos jogados
				val ganhadorDaRodada = perguntarQuemAcertou(numeroDeJogadores)
				ganhadorDaRodada.toInt match {
					// Caso ninguem tenha ganho a rodada (ganhador = jogador 0), começamos outra rodada sem modificar o número de palitos
					case 0 =>
						começarRodada(jogadores, if (primeiroJogador < numeroDeJogadores) primeiroJogador + 1 else 1, numero + 1)
					// Caso alguem tenha ganho, retiro um palito do ganhador e verifico se alguem ficou com 0 palitos
					case _ =>
						// Retiro um palitos do jogador que ganhou e se ele ficou com 0 palitos também removidos ele da lista de jogadores
						val jogadoresComPalitosAtualizados = retirarPalito(jogadores, ganhadorDaRodada)
						// Agora que a lista de jogadores e o número de palitos de cada jogador atualizados, começo uma nova rodada
						começarRodada(jogadoresComPalitosAtualizados, if (primeiroJogador < numeroDeJogadores) primeiroJogador + 1 else 1, numero + 1)
				}
		}
	}

	// Decido qual será minha aposta
	def decidirAposta(jogadores: List[Jogador], apostasDosOutros: List[Int], minhaJogada: Int): Int = {
		val totalDePalitos: Int = palitosDosJogadores(jogadores).sum
		val meusPalitos: Int = jogadores.head.palitos
		apostasDosOutros.size match {
			case x if x <= jogadores.size & x > 0 =>
				// Se eu não for o primeiro a apostar então:
				val rand: Random = new Random()
				// quanto mais jogadores apostarem depois de mim, maior será a minha chance de blefar
				val chanceDeBlefe: Double = apostasDosOutros.size.toDouble / jogadores.size.toDouble
				val random = rand.nextDouble
				// Comparo se a chance de blefe é menor que um número randomico
				 random < chanceDeBlefe match {
					// 	Se sim
					case true =>
						// Definindo algumas funções que serão usadas nesse case
						def blefarPraCima: Int = {
							val minhaAposta = (gerarPorcentagemMaiorQue(0.7) * totalDePalitos).toInt
							apostasDosOutros.contains(minhaAposta) match {
								case false =>
									rand.nextDouble() match {
										case randomDouble if randomDouble > 0.5 => minhaAposta
										case _ => minhaAposta + 1
									}
								case true => blefarPraCima

							}
						}
						def blefarPraBaixo: Int = {
							val minhaAposta = (gerarPorcentagemMenorQue(0.30) * totalDePalitos).toInt
							apostasDosOutros.contains(minhaAposta) match {
								case false =>
									rand.nextDouble() match {
										case randomDouble if randomDouble > 0.5 => minhaAposta
										case _ => minhaAposta + 1
									}
								case true => blefarPraBaixo

							}
						}
						def blefarPraCimaOuPraBaixo: Int = {
							rand.nextDouble() match {
								// 50% de chance de blefar
								case x2 if x2 < 0.5 =>
									blefarPraCima
								// 50% de chance de blefar pra baixo
								case x2 if x2 > 0.5 =>
									blefarPraBaixo
							}
						}
						// Fim das definições

						// Se minha jogada for:
						minhaJogada match {
							case 0 =>
								// Se for 0, blefo fingindo que joguei bastante
								blefarPraCima
							case x1 if x1 > (meusPalitos * 0.7).toInt =>
								// Se for mais que 70% dos meus palitos, vou fingir que joguei pouco
								blefarPraBaixo
							case x1 if x1 < (meusPalitos * 0.3).toInt =>
								// Se for menos que 30% dos meus palitos, vou fingir que joguei bastante
								(gerarPorcentagemMaiorQue(0.70) * totalDePalitos).toInt
							case _ =>
								// Caso seja um número de palitos entre 30% e 70% dos meus palitos, posso fingir que joguei pouco ou bastante
								blefarPraCimaOuPraBaixo
						}
					// Se não
					case false =>
						// Não blefo e faço uma aposta semi-randomica
						apostaRandomica(jogadores, minhaJogada, apostasDosOutros)
				}
			case 0 =>
				// Se eu for o primeiro a apostar faço uma aposta semi-randomica
				apostaRandomica(jogadores, minhaJogada, apostasDosOutros)
			case _ =>
				// Se por algum motivo for outra coisa ocorreu um erro então retorno -1
				-1
		}
	}

	// Uma totalmente randomica, mas válida
	def apostaTotalmenteRandomica(jogadores: List[Jogador], minhaJogada: Int, apostasDosOutros: List[Int]): Int = {
		val rand = new Random
		// Total de palitos dos outros jogadores + minha jogada
		val totalDePalitos: Int = palitosDosJogadores(jogadores.tail).sum + minhaJogada
		val minhaAposta = rand.nextInt(totalDePalitos - minhaJogada + 1) + minhaJogada
		apostasDosOutros.contains(minhaAposta) match {
			case false => minhaAposta
			case true => apostaTotalmenteRandomica(jogadores, minhaJogada, apostasDosOutros)
		}
	}

	// Faço uma aposta randomica mas que possui alguma lógica. OBS: Mesmo sendo randômica a jogada sempre é valida ... não apostaria 0 sendo que joguei 1 palito
	def apostaRandomica(jogadores: List[Jogador], minhaJogada: Int, apostasDosOutros: List[Int], tentativas: Int = 1): Int = {
		val rand = new Random
		// Total de palitos dos outros jogadores + minha jogada
		val totalDePalitosDosOutros: Int = palitosDosJogadores(jogadores.tail).sum
		// A minha aposta depende do número total de palitos
		val minhaAposta = (totalDePalitosDosOutros match {
			// Caso o total de palitos seja par e maior que 9
			case x if isEven(x) && totalDePalitosDosOutros > 9 =>
				// A minha aposta será a metade do número total de palitos +/- 2
				(totalDePalitosDosOutros / 2) + (rand.nextInt(5) - 2) + 1
			// Caso o total de palitos seja par, maior ou igual a 3 e menor ou igual a 9
			case x if isEven(x) && totalDePalitosDosOutros >= 3 && totalDePalitosDosOutros <= 9 =>
				// A minha aposta será a metade do número total de palitos +/- 1
				(totalDePalitosDosOutros / 2) + (rand.nextInt(3) - 1) + 1
			// Caso o total de palitos seja par e menor que 3
			case x if isEven(x) && totalDePalitosDosOutros < 3 =>
				// A minha aposta será totalmente randomica
				rand.nextInt(totalDePalitosDosOutros + 1)
			// Caso o total de palitos seja impar e maior que 9
			case x if isOdd(x) && totalDePalitosDosOutros > 9 =>
				// A minha aposta será a metade do número total de palitos +/- 2
				(totalDePalitosDosOutros / 2) + (rand.nextInt(5) - 2)
			// Caso o total de palitos seja impar, maior ou igual a 3 e menor ou igual a 9
			case x if isOdd(x) && totalDePalitosDosOutros >= 3 && totalDePalitosDosOutros <= 9 =>
				// A minha aposta será a metade do número total de palitos +/- 2
				(totalDePalitosDosOutros / 2) + (rand.nextInt(3) - 1)
			// Caso o total de palitos seja impar e menor que 3
			case x if isOdd(x) && totalDePalitosDosOutros < 3 =>
				// A minha aposta será totalmente randomica
				rand.nextInt(totalDePalitosDosOutros + 1)
		}) + minhaJogada // è adicionado o valor da minha jogada a minha aposta pois primeiramente
		// a aposta se baseia apenas no numero de palitos dos adversários

		// Verifico se nenhum outro jogador já fez essa aposta
		apostasDosOutros.contains(minhaAposta) match {
			// Se não, então confirmo a aposta
			case false => minhaAposta
			// Se alguem já fez essa aposta e eu ainda não tentei muitas apostas, então faço outra aposta randomica
			case x if x == true && tentativas <= 5 => apostaRandomica(jogadores, minhaJogada, apostasDosOutros, tentativas + 1)
			// Se alguem já fez essa aposta e já tentei uma aposta usando a lógica acima várias vezes,
			// então faço uma aposta totalmente randomica
			case x if x == true && tentativas > 5 => apostaTotalmenteRandomica(jogadores, minhaJogada, apostasDosOutros)
		}
	}

	// Jogo um numero X de palitos sendo que -1 < X <= meusPalitos
	def jogadaRandomica(palitos: Int): Int = {
		val rand = new Random
		rand.nextInt(palitos + 1)
	}

	// Gero uma porcentagem menos que o double passado como parametro ... se 0.30 for passado como parametro retornarei
	// um double x tal que 0 < X < 0.30
	def gerarPorcentagemMenorQue(quanto: Double): Double = {
		val rand = new Random
		rand.nextDouble() match {
			case x if x < quanto => x
			case _ => gerarPorcentagemMaiorQue(quanto)
		}
	}

	// Gero uma porcentagem maior que o double passado como parametro ... se 0.70 for passado como parametro retornarei
	// um double x tal que 0.30 < X <= 1.0
	def gerarPorcentagemMaiorQue(quanto: Double): Double = {
		val rand = new Random
		rand.nextDouble() match {
			case x if x > quanto => x
			case _ => gerarPorcentagemMaiorQue(quanto)
		}
	}

	// Verifico se algum jogador está com 0 palitos (foi eliminado)
	def verificarSeAlguemFoiEliminado(jogadores: List[Jogador], index: Int = 0): Int = {
		jogadores.head.palitos match {
			case 0 => index
			case _ =>
				jogadores.tail.size match {
					case x if x > 0 => verificarSeAlguemFoiEliminado(jogadores.tail, index + 1)
					case _ => -1
				}
		}
	}

	// Retorno uma lista com o número de palitos de cada jogador
	def palitosDosJogadores(jogadores: List[Jogador]): List[Int] = {
		jogadores.map(jogador => jogador.palitos)
	}

	// Retiro o palito do jogador com ID igual ao valor do parametro 'ganhadorDaRodada'. Se o seu numero de palitos zerar
	// ele é removido do jogo
	def retirarPalito(jogadores: List[Jogador], ganhadorDaRodada: String): List[Jogador] = {
		// Verifico existe um jogador com o mesmo ID igual ao valor do parametro ganhadorDaRodada na lista de jogadores
		jogadores.exists(jogador => jogador.ID == ganhadorDaRodada) match {
			case true =>
				// Caso exista, entao retiro um palito desse jogador
				val ganhador = jogadores.filter(jogador => jogador.ID == ganhadorDaRodada).head
				ganhador.palitos match {
					// Se o jogador tinha apenas um palito, entao ele vai ficar com 0 palitos e sera removido do jogo
					case 1 =>
						val index = jogadores.indexOf(ganhador)
						jogadores.take(index) ++ jogadores.drop(index + 1)
					// Se ele tiver qualquer outro numero de palitos entao nao sera necessario remove-lo do jogo
					// apenas atualizarei o seu numero de palitos
					case _ =>
						val index = jogadores.indexOf(ganhador)
						(jogadores.take(index) :+ new Jogador(ganhador.palitos - 1, ganhador.ID)) ++ jogadores.drop(index + 1)
				}
			case false =>
				// Caso nao existe entao nao faço nada e apenas retorno os mesmos jogadores que recebi
				jogadores
		}
	}

	// Gera um número random entre 1 e numeroDeJogadores
	def decidirQuemJogaPrimeiro(numeroDeJogadores: Int): Int = {
		val rand = new Random
		rand.nextInt(numeroDeJogadores) + 1
	}

	// Cria uma lista com com uma entrada pra cada jogador com valor 3, que representa o número inicial de palitos
	def criarJogadores(numeroDePalitos: Int, numeroDeJogadores: Int, index: Int = 0, listaDeJogadores: List[Jogador] = List()): List[Jogador] = {
		index match {
			case x if x >= numeroDeJogadores => listaDeJogadores
			case _ => criarJogadores(numeroDePalitos, numeroDeJogadores, index + 1, listaDeJogadores :+ new Jogador(numeroDePalitos, (index + 1).toString))
		}
	}

	// Digo algo
	def dizer(algo: String): Unit = {
		println(algo)
	}

	// Fico perguntando se a rodada já acabou até alguem inserir o indice do ganhador ou 0 caso ngm tenha acertado
	def perguntarQuemAcertou(numeroDeJogadores: Int): String = {
		try {
			val input = readLine("Se alguem acertou o número de palitos informe o número desse jogador. Se ninguem acertou digite 0: \tR: ")
			print("\n")
			println("O jogador " + input + " ganhou esta rodada!")
			print("\n")
			input.toInt match {
				case x if x >= 0 | x <= numeroDeJogadores =>
					input.toString
				case _ =>
					println("Você deve inserir um numero positivo positivo")
					perguntarQuemAcertou(numeroDeJogadores)
			}
		}
		catch {
			case nfe: NumberFormatException =>
				println("Você deve inserir um número")
				perguntarQuemAcertou(numeroDeJogadores)
			case npe: NullPointerException =>
				println("Você deve inserir um número")
				perguntarQuemAcertou(numeroDeJogadores)
		}
	}

	// Fico perguntado até que a resposta seja 'sim'
	def perguntarAteSim(pergunta: String, resposta: String = "não"): Unit = {
		resposta match {
			case "sim" =>
			case "não" => perguntarSimOuNao(pergunta)
		}
	}

	// Faço uma pergunta até que a resposta seja 'sim' ou 'não'
	def perguntarSimOuNao(pergunta: String): String = {
		try {
			val resposta = readLine(pergunta)
			resposta match {
				case "sim" => "sim"
				case "não" => "não"
				case _ =>
					println("Diga <sim/não>")
					perguntarSimOuNao(pergunta)
			}
		}
		catch {
			case npe: NullPointerException =>
				println("Diga <sim/não>")
				perguntarSimOuNao(pergunta)
		}
	}

	// Lêio as apostas dos jogadores que jogam antes do computador
	def lerApostasDeCadaJogador(jogadores: List[Jogador], primeiroJogador: Int): List[Int] = {

		// Encontro os jogadores que jogam antes da maquina e retorno uma lista com seus IDS
		def quemJogaAntesDaMaquina(lista: List[String] = List(), index: Int = 0): List[String] = {
			index + 1 match {
				case x if x < primeiroJogador =>
					quemJogaAntesDaMaquina(lista, index + 1)
				case x if x >= primeiroJogador & x <= jogadores.size =>
					quemJogaAntesDaMaquina(lista :+ jogadores(index).ID, index + 1)
				case _ =>
					lista
			}
		}
		try {
			val apostadores = quemJogaAntesDaMaquina()
			apostadores.size match {
				case 0 =>
					List()
				case _ =>
					println("O(s) jogadore(s) que joga(m) antes do computador são:\n")
					apostadores.foreach(jogador => println("-> Jogador " + jogador))
					val input = readLine("\nInforme sua(s) aposta(s) no formato <a1,a2,..,ai>, começando com o jogador de número mais baixo: \nR: ")
					print("\n")
					input.split(",").length match {
						case x if x == apostadores.size =>
							val listaDeApostas = input.split(",").toList.map(x => x.toInt)
							listaDeApostas.size match {
								case 1 =>
									listaDeApostas
								case _ =>
									listaDeApostas.distinct.size match {
										case y if y == listaDeApostas.size =>
											listaDeApostas
										case _ =>
											println("Não são aceitas apostas repetidas, vamos denovo")
											lerApostasDeCadaJogador(jogadores, primeiroJogador)
									}
							}
						case _ =>
							println("Os dados inseridos estavam incorretos, vamos denovo")
							lerApostasDeCadaJogador(jogadores, primeiroJogador)
					}
			}
		}
		catch {
			case nfe: NumberFormatException =>
				println("Você deve inserir um número")
				lerApostasDeCadaJogador(jogadores, primeiroJogador)
			case npe: NullPointerException =>
				println("Você deve inserir um número")
				lerApostasDeCadaJogador(jogadores, primeiroJogador)
		}
	}

	// Lêio do console o número de jogadores que irão jogar
	def lerNumeroDeJogadores: Int = {
		try {
			val input = readLine("\nInforme o número de jogadores: \tR: ")
			print("\n")
			input.toInt match {
				case x if x > 0 =>
					input.toInt
				case _ =>
					println("Você deve inserir um numero positivo positivo")
					lerNumeroDeJogadores
			}
		}
		catch {
			case nfe: NumberFormatException =>
				println("Você deve inserir um número")
				lerNumeroDeJogadores
			case npe: NullPointerException =>
				println("Você deve inserir um número")
				lerNumeroDeJogadores
		}
	}

	def isEven(v: Int): Boolean = v % 2 == 0

	def isOdd(v: Int): Boolean = v % 2 != 0
}
