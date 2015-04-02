
import scala.util.Random

object Application {
	def main(args: Array[String]) {
		println("\n\n***********************************************************************************************************")

		// SEMPRE sou o jogador número 1
		val numeroDeJogadores = lerNumeroDeJogadores
		// Começo a primeira rodada
		começarRodada(criarJogadores(3,numeroDeJogadores), decidirQuemJogaPrimeiro(numeroDeJogadores))
	}

	// Começo uma nova rodada do jogo
	def começarRodada(jogadores : List[Jogador], primeiroJogador : Int): Unit = {
		jogadores.size match
		{
			case 1 =>
				println("Jogador número "+jogadores.head.ID+" você ficou em último lugar, shame on you \n")
				println("      #################")
				println("      ## FIM DE JOGO ##")
				println("      #################")
			case _ =>
				val numeroDeJogadores = jogadores.size
				dizerInformacoesSobreARodada()
				primeiroJogador match
				{
					case 1 =>
						jogarPrimeiro()
					case _ =>
						jogar()
				}


				// Quando jogo primeiro não preciso esperar as informações sobre as apostas dos outros jogadores
				def jogarPrimeiro(): Unit ={
					val minhaJogada = jogadaRandomica( jogadores.head.palitos)
					val minhaAposta = decidirAposta(jogadores, List(), minhaJogada)
					pedirParaJogar()
					pedirPraMostrarAposta()
					dizerAposta(minhaAposta)
					esperarFimDaRodada()
					dizerJogada(minhaJogada)
					val ganhadorDaRodada = perguntarQuemAcertou(numeroDeJogadores)
					ganhadorDaRodada match
					{
						case 0 =>
							começarRodada(jogadores, if (primeiroJogador < numeroDeJogadores) primeiroJogador+1 else 1 )
						case _ =>
							val jogadoresComPalitosAtualizados = retirarPalito(jogadores, ganhadorDaRodada)
							verificarSeAlguemFoiEliminado(jogadoresComPalitosAtualizados) match
							{
								case -1 =>
									começarRodada(jogadoresComPalitosAtualizados, if (primeiroJogador < numeroDeJogadores) primeiroJogador+1 else 1 )
								case x =>
									começarRodada(jogadoresComPalitosAtualizados.take(x) ++ jogadoresComPalitosAtualizados.drop(x+1), if (primeiroJogador < numeroDeJogadores) primeiroJogador+1 else 1 )
							}
					}
				}

				// Se eu não for o primeiro a jogar, primeiro preciso das informações sobre as apostas dos outros jogadores
				// pra então fazer a minha aposta
				def jogar(): Unit = {
					val apostas = lerApostasDeCadaJogador(jogadores, primeiroJogador)
					apostas.size match
					{
						case tamanho if tamanho > 0 =>
							val minhaJogada = jogadaRandomica( jogadores.head.palitos )
							val minhaAposta = decidirAposta(jogadores, apostas, minhaJogada)
							pedirPraMostrarAposta()
							dizerAposta(minhaAposta)
							esperarFimDaRodada()
							dizerJogada(minhaJogada)
							val ganhadorDaRodada = perguntarQuemAcertou(numeroDeJogadores)
							ganhadorDaRodada match
							{
								case 0 =>
									começarRodada(jogadores, if (primeiroJogador < numeroDeJogadores) primeiroJogador+1 else 1 )
								case _ =>
									val jogadoresComPalitosAtualizados = retirarPalito(jogadores, ganhadorDaRodada)
									verificarSeAlguemFoiEliminado(jogadoresComPalitosAtualizados) match
									{
										case -1 =>
											começarRodada(jogadoresComPalitosAtualizados, if (primeiroJogador < numeroDeJogadores) primeiroJogador+1 else 1 )
										case x =>
											começarRodada(jogadoresComPalitosAtualizados, if (primeiroJogador < numeroDeJogadores) primeiroJogador+1 else 1 )
									}
							}
						case _ =>
							println( "Algo deu muito errado!")
					}
				}
				def dizerInformacoesSobreARodada(): Unit =
				{
					println("Começando uma nova rodada                                            \n")
					println("Status dos jogadores:")
					jogadores.foreach( jogador => println("Jogador "+ jogador.ID + " => "+jogador.palitos+" palitos"))
					println("\nPrimeiro a jogar => Jogador " + primeiroJogador)
					println("***********************************************************************************************************")
				}
		}
	}

	// Decido qual será minha aposta
	def decidirAposta (jogadores : List[Jogador] , apostasDosOutros : List[Int], minhaJogada : Int): Int = {
		val totalDePalitos : Int  = palitosDosJogadores( jogadores ).sum
		val meusPalitos : Int = jogadores.head.palitos
		apostasDosOutros.size match
		{
			case x if x <= jogadores.size & x > 0 =>
				// Se eu não for o primeiro a apostar então:
				val rand : Random = new Random()
				// quanto mais jogadores apostarem depois de mim, maior será a minha chance de blefar
				val chanceDeBlefe : Double = apostasDosOutros.size.toDouble / jogadores.size.toDouble-1
				rand.nextDouble > chanceDeBlefe match
				{
					case true  =>
						def blefarPraCima : Int = {
							val minhaAposta = (gerarPorcentagemMaiorQue(0.7)*totalDePalitos).toInt
							apostasDosOutros.contains( minhaAposta ) match
							{
								case false =>
									rand.nextDouble() match
									{
										case randomDouble if randomDouble > 0.5 => minhaAposta
										case _ => minhaAposta+1
									}
								case true  => blefarPraCima

							}
						}
						def blefarPraBaixo : Int = {
							val minhaAposta = (gerarPorcentagemMenorQue(0.30)*totalDePalitos).toInt
							apostasDosOutros.contains( minhaAposta ) match
							{
								case false =>
									rand.nextDouble() match
									{
										case randomDouble if randomDouble > 0.5 => minhaAposta
										case _ => minhaAposta+1
									}
								case true  => blefarPraBaixo

							}
						}
						def blefarPraCimaOuPraBaixo : Int = {
							rand.nextDouble() match
							{
								// 50% de chance de blefar
								case x2 if x2 < 0.5 =>
									blefarPraCima
								// 50% de chance de blefar pra baixo
								case x2 if x2 > 0.5 =>
									blefarPraBaixo
							}
						}
						minhaJogada match
						{
							case 0 =>
								// Se eu joguei 0 blefo, fingindo que joguei bastante
								blefarPraCima
							case x1 if x1 > (meusPalitos*0.7).toInt =>
								// Se joguei mais que 70% dos meus palitos, vou fingir que joguei pouco
								blefarPraBaixo
							case x1 if x1 < (meusPalitos*0.3).toInt =>
								// Se joguei menos que 30% dos meus palitos, vou fingir que joguei bastante
								(gerarPorcentagemMaiorQue(0.70)*totalDePalitos).toInt
							case _ =>
								// Caso tenha jogado um número de palitos entre 30% e 70% dos meus palitos, posso fingir que joguei pouco ou bastante
								blefarPraCimaOuPraBaixo
						}
					case false =>
						// Quando não blefo faço uma jogada randomica
						apostaRandomica( jogadores, minhaJogada, apostasDosOutros)

				}
			case 0 =>
				// Se eu for o primeiro a apostar faço uma aposta randomica
				apostaRandomica( jogadores, minhaJogada, apostasDosOutros)
			case _ =>
				// Se por algum motivo for outra coisa ocorreu um erro então retorno -1
				-1
		}
	}

	// Faço uma aposta randomica. OBS: Mesmo sendo randômica a jogada sempre é valida ... não apostaria 0 sendo que joguei 1 palito
	def apostaRandomica(jogadores : List[Jogador], minhaJogada : Int,apostasDosOutros : List[Int]) : Int =  {
		val rand = new Random
		// Total de palitos dos outros jogadores + minha jogada
		val totalDePalitos : Int = palitosDosJogadores( jogadores.tail ).sum + minhaJogada
		val minhaAposta = rand.nextInt(totalDePalitos-minhaJogada+1)+minhaJogada
		apostasDosOutros.contains( minhaAposta ) match
		{
			case false => minhaAposta
			case true  => apostaRandomica(jogadores, minhaJogada,apostasDosOutros)
		}
	}

	// Jogo um numero X de palitos sendo que -1 < X <= meusPalitos
	def jogadaRandomica( palitos : Int ): Int ={
		val rand = new Random
		rand.nextInt(palitos+1)
	}

	// Gero uma porcentagem menos que o double passado como parametro ... se 0.30 for passado como parametro retornarei
	// um double x tal que 0 < X < 0.30
	def gerarPorcentagemMenorQue( quanto : Double ): Double =
	{
		val rand = new Random
		rand.nextDouble() match
		{
			case x if x < quanto => x
			case _ => gerarPorcentagemMaiorQue( quanto )
		}
	}

	// Gero uma porcentagem maior que o double passado como parametro ... se 0.70 for passado como parametro retornarei
	// um double x tal que 0.30 < X <= 1.0
	def gerarPorcentagemMaiorQue( quanto : Double ): Double =
	{
		val rand = new Random
		rand.nextDouble() match
		{
			case x if x > quanto => x
			case _ => gerarPorcentagemMaiorQue( quanto )
		}
	}

	// Verifico se algum jogador está com 0 palitos (foi eliminado)
	def verificarSeAlguemFoiEliminado(jogadores : List[Jogador], index : Int = 0): Int ={
		jogadores.head.palitos match
		{
			case 0 => index
			case _ =>
				jogadores.tail.size match
				{
					case x if x > 0 => verificarSeAlguemFoiEliminado(jogadores.tail, index+1)
					case _ => -1
				}
		}
	}

	// Retorno uma lista com o número de palitos de cada jogador
	def palitosDosJogadores( jogadores : List[Jogador]): List[Int] ={
		jogadores.map(jogador => jogador.palitos)
	}

	// Retiro o palito do jogador com ID igual ao valor do parametro 'ganhadorDaRodada'. Se o seu numero de palitos zerar
	// ele é removido do jogo
	def retirarPalito(jogadores : List[Jogador], ganhadorDaRodada : Int) : List[Jogador] ={
		val lista = jogadores.filter( jogador => jogador.ID == ganhadorDaRodada.toString)
		lista.size match
		{
			case 1 =>
				// Caso a lista possui apenas um elemento
				lista.head.palitos match
				{
					case 0 =>
						// Retorno a lista sem o jogador, pois ele ficou com 0 palitos
						val index = jogadores.indexOf( lista.head )
						jogadores.take(index) ++ jogadores.drop(index+1)
					case numeroDePalitos if numeroDePalitos > 0 =>
						// Retorno uma nova lista de jogador praticamente igual outra mas onde o ganhador da rodada possui um palito a menos
						val index = jogadores.indexOf( lista.head )
						(jogadores.take(index) :+ new Jogador(lista.head.palitos-1, lista.head.ID)) ++ jogadores.drop(index+1)
				}
			case _ =>
				// Caso a lista possua mais de um elemento ( O que nunca deve acontecer a não ser que 2 ou mais jogadores possuem o mesmo nome)
				// Retorno uma lista vazia
				List()
		}

	}
	// Fico pedindo pra jogar até alguem responder 'sim'
	def pedirParaJogar(): Unit ={
		try {
			var resposta = "nao"
			while( resposta != "sim"){
				resposta = readLine("Nesta rodada eu sou o primeiro a jogar, todos já fizeram suas jogadas? Posso começar a rodada? \tR: ")
				print("\n")
			}
		}
		catch {
			case npe: NullPointerException =>
				println("Diga <sim>"+ "\n")
				lerNumeroDeJogadores
		}
	}

	// Fico perguntando se a rodada já acabou até alguem inserir o indice do ganhador ou 0 caso ngm tenha acertado
	def perguntarQuemAcertou(numeroDeJogadores : Int) : Int ={
		try {
			val input = readLine("Se alguem acertou o número de palitos informe o número desse jogador. Se ninguem acertou digite 0: \tR: ")
			print("\n")
			println("*********************************************************************************************************** ")
			input.toInt match {
				case x if x >= 0 | x <= numeroDeJogadores =>
					input.toInt
				case _ =>
					println("Você deve inserir um numero positivo positivo"+ "\n")
					perguntarQuemAcertou(numeroDeJogadores)
			}
		}
		catch {
			case nfe: NumberFormatException =>
				println("Você deve inserir um número"+ "\n")
				perguntarQuemAcertou(numeroDeJogadores)
			case npe: NullPointerException =>
				println("Você deve inserir um número"+ "\n")
				perguntarQuemAcertou(numeroDeJogadores)
		}
	}

	// Digo qual foi minha jogada
	def dizerJogada( jogada : Int ): Unit ={
		println("Minha jogada é: " + jogada + "\n")
	}

	// Digo qual foi minha aposta
	def dizerAposta( aposta : Int): Unit ={
		println("Minha aposta é: "+aposta + "\n")
	}

	// Fico perguntado se a rodada já acabou até alguem responder 'sim'
	def pedirPraMostrarAposta(): Unit ={
		try {
			var resposta = "nao"
			while( resposta != "sim"){
				resposta = readLine("Posso mostrar minha aposta? \tR: ")
				print("\n")
			}
		}
		catch {
			case npe: NullPointerException =>
				println("Diga <sim>"+ "\n")
				lerNumeroDeJogadores
		}
	}

	// Fico perguntado se a rodada já acabou até alguem responder 'sim'
	def esperarFimDaRodada(): Unit ={
		try {
			var resposta = "nao"
			while( resposta != "sim"){
				resposta = readLine("Posso mostrar minha jogada? \tR: ")
				print("\n")
			}
		}
		catch {
			case npe: NullPointerException =>
				println("Diga <sim>"+ "\n")
				lerNumeroDeJogadores
		}
	}

	// Cria uma lista com com uma entrada pra cada jogador com valor 3, que representa o número inicial de palitos
	def criarJogadores(numeroDePalitos : Int, numeroDeJogadores: Int, index : Int = 0, listaDeJogadores: List[Jogador] = List()): List[Jogador] = {
		index match
		{
			case x if x >= numeroDeJogadores => listaDeJogadores
			case _ => criarJogadores(numeroDePalitos, numeroDeJogadores, index+1, listaDeJogadores :+ new Jogador(numeroDePalitos,(index+1).toString))
		}
	}

	// Gera um número random entre 1 e numeroDeJogadores
	def decidirQuemJogaPrimeiro(numeroDeJogadores: Int): Int = {
		val rand = new Random
		rand.nextInt(numeroDeJogadores) + 1
	}


	// Lêio as apostas dos jogadores que jogam antes do computador
	def lerApostasDeCadaJogador(jogadores : List[Jogador], primeiroJogador: Int): List[Int] = {

		// Encontro os jogadores que jogam antes da maquina e retorno uma lista com seus IDS
		def quemJogaAntesDaMaquina (lista: List[String] = List(), index : Int = 0): List[String] = {
			index+1 match {
				case x if x < primeiroJogador =>
					quemJogaAntesDaMaquina(lista, index+1)
				case x if x >= primeiroJogador & x <= jogadores.size =>
					quemJogaAntesDaMaquina(lista :+ jogadores(index).ID, index+1)
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
					apostadores.foreach(jogador => println("-> Jogador "+jogador))
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
											println("Não são aceitas apostas repetidas, vamos denovo"+ "\n")
											lerApostasDeCadaJogador(jogadores, primeiroJogador)
									}
							}
						case _ =>
							println("Os dados inseridos estavam incorretos, vamos denovo"+ "\n")
							lerApostasDeCadaJogador(jogadores, primeiroJogador)
					}
			}
		}
		catch {
			case nfe: NumberFormatException =>
				println("Você deve inserir um número"+ "\n")
				lerApostasDeCadaJogador(jogadores, primeiroJogador)
			case npe: NullPointerException =>
				println("Você deve inserir um número"+ "\n")
				lerApostasDeCadaJogador(jogadores, primeiroJogador)
		}
	}

	// Lêio do console o número de jogadores que irão jogar
	def lerNumeroDeJogadores: Int = {
		try {
			val input = readLine("Informe o número de jogadores: \tR: ")
			print("\n")
			println("***********************************************************************************************************")
			input.toInt match {
				case x if x > 0 =>
					input.toInt
				case _ =>
					println("Você deve inserir um numero positivo positivo"+ "\n")
					lerNumeroDeJogadores
			}
		}
		catch {
			case nfe: NumberFormatException =>
				println("Você deve inserir um número"+ "\n")
				lerNumeroDeJogadores
			case npe: NullPointerException =>
				println("Você deve inserir um número"+ "\n")
				lerNumeroDeJogadores
		}
	}
}
