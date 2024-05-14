object DE_45_KVV {

  // def hescala(): Unit =
  def main(args: Array[String]): Unit = {}

  def hello(): Unit = {     // программа согласно пункта 3.a задания
    val phraseHello = "Hello, Scala!"
    val phraseGoodbye= "and goodbye python!"
    println(phraseHello)
    println(phraseHello.reverse)            // выводит фразу «Hello, Scala !», справа налево
    println(phraseHello.toLowerCase())      // переводит всю фразу в нижний регистр
    println(phraseHello.replace("!", ""))       // удаляет символ !
    println(phraseHello.replace("!", " ").concat(phraseGoodbye))      // добавляет в конец фразы «and goodbye python!»
  }
  hello()   // запуск функции hello

  /*
  3.b.	Напишите программу, которая вычисляет ежемесячный оклад сотрудника после вычета налогов.
  На вход вашей программе подается:
    - значение годового дохода до вычета налогов,
    - размер премии  — в процентах от годового дохода и
    - компенсация питания.
   */
  def monthlySalary(annualIncome: Double, bonus: Double, mealCompensation: Double, tax: Double): Unit = {
    var incomeWithoutMealCompensation = annualIncome - mealCompensation * 12  //вычитаем компенсацию питания, т.к. это не подлежит налогообложению
    var incomeAfterTaxes = incomeWithoutMealCompensation * (1 - tax / 100)    // вычисляем годовой доход после вычета налогов
    var incomeWithoutBonus = incomeAfterTaxes - (incomeAfterTaxes * (1 - bonus / 100))  // вычитаем премию
    var monthlyIncome = incomeWithoutBonus / 12     // вычисляем ежемесячный оклад сотрудника после вычета налогов
    println("Размер ежемесячного оклада составляет: " + monthlyIncome.round)
    // println(f"Размер ежемесячного оклада составляет:  $clearMonthlySalary%.2f")
  }
  monthlySalary(4000, 50, 100, 13) // вызов функции. решающей задачу согласно пункта 3b

  /*
  3.c.	Напишите программу, которая рассчитывает для каждого сотрудника отклонение (в процентах)
  от среднего значения оклада на уровне всего отдела.
  В итоговом значении должно учитываться в большую или меньшую сторону отклонение размера оклада.
  На вход вашей программе подаются все значения, аналогичные предыдущей программе,
  а также список со значениями окладов сотрудников отдела 100, 150, 200, 80, 120, 75.
   */

  def salaryDeviation(salaries: List[Double]) = {
    val avrSalary = salaries.sum / salaries.length.toDouble    // вычисляем средний оклад
    println(s"Средний оклад составляет:  $avrSalary")
    val avrDevSalary = salaries.map(salar => (salar / avrSalary - 1) * 100).mkString(", ")
    // val avrDevSalaryRound = avrDevSalary
    println(s"Отклонение среднего значения оклада на уровне отдела составляет (в процентах):  \n $avrDevSalary")
  }
  val salaries = List(100.00, 150.00, 200.00, 80.00, 120.00, 75.00)
  salaryDeviation(salaries) //  вызов функции. решающей задачу согласно пункта 3c

  /*
  3.d.	Попробуйте рассчитать новую зарплату сотрудника, добавив (или отняв, если сотрудник плохо себя вел)
  необходимую сумму с учетом результатов прошлого задания.
  Добавьте его зарплату в список и вычислите значение самой высокой зарплаты и самой низкой.
   */

  def corBonus(salaries: List[Double], correction: List[Double]) = {
    var resultSalary = (salaries, correction).zipped.map(_ * _)
    println("Зарплата после корректировки составляет: " + resultSalary.mkString(", "))
  }
  val correctionSalary = List(1.2, 1,1, 0.9, 1, 0.8, 1.15)    //  коэффициенты для корректировки премий
  corBonus(salaries, correctionSalary: List[Double]) // вызов функции. решающей задачу согласно пункта 3d

  /*
  3.e.	Также в вашу команду пришли два специалиста с окладами 350 и 90 тысяч рублей.
  Попробуйте отсортировать список сотрудников по уровню оклада от меньшего к большему.
   */
  def sort(salaries: List[Double], addSalaries: List[Double]) = {
    println("Отсортированный список зарплат включая новых сотрудников: " + (salaries ::: addSalaries).sorted.mkString(", "))
  }
  val newEmployees = List(350.0, 90.0)
  sort(salaries, newEmployees) //пункт e

  /*
  3.f.	Кажется, вы взяли в вашу команду еще одного сотрудника и предложили ему оклад 130 тысяч.
  Вычислите самостоятельно номер сотрудника в списке так, чтобы сортировка не нарушилась, и добавьте его на это место.
   */
  def addNewEmployeeWithSorted(salaries: List[Double], addSalaries: List[Double], newEmpl: Double) = {
    val sortedAllEmpl = (salaries ::: addSalaries).sorted
    val indexNewEmpl: Int = sortedAllEmpl.indexWhere(sal => sal >= newEmpl)
    val resultAllEmpl = sortedAllEmpl.dropRight(indexNewEmpl - 1) ++ List(newEmpl) ++ sortedAllEmpl.drop(indexNewEmpl)
    var i: Int = 0
    println("Список окладов сотрудников с учётом нового сотрудника (после сортировки): " + resultAllEmpl.mkString(", "))
    //println("Список сотрудников с новым добавленным после сортировки: ")
    /*while (i < resultAllEmpl.length) {
      println(i + ": " + resultAllEmpl(i))
      i += 1
    }*/
  }
  val newEmployee = 130.00
  addNewEmployeeWithSorted(salaries, newEmployees, newEmployee) //пункт f

  /*
  3.g.	Попробуйте вывести номера сотрудников из полученного списка, которые попадают под категорию middle.
  На входе программе подается «вилка» зарплаты специалистов уровня middle.
   */
  def numberOfEmployeeOfMidle(salaries: List[Double], addSalaries: List[Double], newEmpl: Double, rangeMin: Double, rangeMax: Double) = {
    val sortedAllEmpl = (salaries ::: addSalaries).sorted
    val indexNewEmpl: Int = sortedAllEmpl.indexWhere(sal => sal >= newEmpl)
    val resultAllEmpl = sortedAllEmpl.dropRight(indexNewEmpl - 1) ++ List(newEmpl) ++ sortedAllEmpl.drop(indexNewEmpl)
    var i: Int = 0
    println("Список сотрудников, которые попадают под категорию middle: ")
    while (i < resultAllEmpl.length) {
      if (resultAllEmpl(i) > rangeMin && resultAllEmpl(i) < rangeMax) println(i + ": " + resultAllEmpl(i))
      i += 1
    }
  }
  val rangeMin = 100.0      // Нижняя граница для middle
  val rangeMax = 300.0      // Верхняя граница для middle
  numberOfEmployeeOfMidle(salaries, newEmployees, newEmployee, rangeMin, rangeMax) //пункт g

  /*
  3.h.	Однако наступил кризис и ваши сотрудники требуют повысить зарплату.
  Вам необходимо проиндексировать зарплату каждого сотрудника на уровень инфляции  — 7 %.
   */
  def indexingSalary(salaries: List[Double], addSalaries: List[Double], newEmpl: Double, percentIndexing: Double) = {
    val sortedAllEmpl = (salaries ::: addSalaries).sorted
    val indexNewEmpl: Int = sortedAllEmpl.indexWhere(sal => sal >= newEmpl)
    val resultAllEmpl = sortedAllEmpl.dropRight(indexNewEmpl - 1) ++ List(newEmpl) ++ sortedAllEmpl.drop(indexNewEmpl)
    val resultAfterIndexing = resultAllEmpl.map(sal => sal * (1 + percentIndexing / 100))
    var i: Int = 0
    println("Список зарплат сотрудников после индексирования: ")
    while (i < resultAfterIndexing.length) {
      println(i + ": " + resultAfterIndexing(i))
      i += 1
    }
  }
  val percentIndexing = 7.0   // процент индексации зарплаты
  indexingSalary(salaries, newEmployees, newEmployee, percentIndexing) //пункт h

}


