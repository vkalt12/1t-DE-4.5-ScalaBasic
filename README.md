# 1t-DE-4.5-ScalaBasic
Задание на курсах 1t "Инженер данных". 4.5 Основы работы Scala

1.	Подготовьте все необходимое для работы с языком Scala: 
a.	Установите Java 8 JDK( Download the Latest Java LTS Free). В принципе этот шаг можно пропустить, т.к. IDEA сама предложит вам установить JDK, если вы вдруг забудете. Но упражнение по самостоятельной установке JDK тоже полезное.
b.	Установите Intellij IDEA( IntelliJ IDEA).
c.	Установите плагин для Scala( Discover IntelliJ IDEA for Scala | IntelliJ IDEA).
d.	Если планируете использовать sbt для сборки вашего проекта  — он уже предустановлен:  Getting Started with Scala in IntelliJ.
e.	Если планируете использовать maven  — подробное руководство по установке и сборке проекта с помощью maven:  Create Scala Project with Maven in IntelliJ IDEA - Java Helps.
2.	Создайте ваш первый проект, который печатает на экран фразу «Hello, Scala!». Запустите ваш код, убедитесь, что он работает и попробуйте собрать ваш первый jar’ник.
3.	Далее нам предстоит потренировать наши навыки работы с языком Scala, чтобы вы смогли привыкнуть к синтаксису языка, его особенностям и познакомились с основными конструкциями. Поэтому предлагаю решить следующие задачки:
a.	Напишите программу, которая: 
i.	выводит фразу «Hello, Scala!» справа налево
ii.	переводит всю фразу в нижний регистр
iii.	удаляет символ!
iv.	добавляет в конец фразы «and goodbye python!»
b.	Напишите программу, которая вычисляет ежемесячный оклад сотрудника после вычета налогов. На вход вашей программе подается значение годового дохода до вычета налогов, размер премии  — в процентах от годового дохода и компенсация питания. 
c.	Напишите программу, которая рассчитывает для каждого сотрудника отклонение (в процентах) от среднего значения оклада на уровне всего отдела. В итоговом значении должно учитываться в большую или меньшую сторону отклонение размера оклада. На вход вашей программе подаются все значения, аналогичные предыдущей программе, а также список со значениями окладов сотрудников отдела 100, 150, 200, 80, 120, 75.
d.	Попробуйте рассчитать новую зарплату сотрудника, добавив (или отняв, если сотрудник плохо себя вел) необходимую сумму с учетом результатов прошлого задания. Добавьте его зарплату в список и вычислите значение самой высокой зарплаты и самой низкой. 
e.	Также в вашу команду пришли два специалиста с окладами 350 и 90 тысяч рублей. Попробуйте отсортировать список сотрудников по уровню оклада от меньшего к большему. 
f.	Кажется, вы взяли в вашу команду еще одного сотрудника и предложили ему оклад 130 тысяч. Вычислите самостоятельно номер сотрудника в списке так, чтобы сортировка не нарушилась, и добавьте его на это место.
g.	Попробуйте вывести номера сотрудников из полученного списка, которые попадают под категорию middle. На входе программе подается «вилка» зарплаты специалистов уровня middle.
h.	Однако наступил кризис и ваши сотрудники требуют повысить зарплату. Вам необходимо проиндексировать зарплату каждого сотрудника на уровень инфляции  — 7 %.
4.	Т.к. большинство задач взаимосвязаны, вы можете остаться в рамках одного проекта/класса/объекта и записать все решения там. По итогам работы вам необходимо запушить проект в отдельный репозиторий github и приложить ссылку.  
Результат выполнения задания необходимо выложить в github/gitlab и указать ссылку на Ваш репозиторий (не забудьте: репозиторий должен быть публичным).
