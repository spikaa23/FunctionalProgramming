import org.scalatest.funsuite.AnyFunSuite

// Тестовий клас, який перевіряє логіку з Workshop.scala
class WorkshopSpec extends AnyFunSuite:

  // Запускається командою: sbt test
  
  test("Функція categorize повинна правильно визначати рівні ризику"):
   // 1. Перевірка високого ризику (> 80.0)
    assert(Workshop.categorize(90.0) == Workshop.HighRisk)
    assert(Workshop.categorize(80.01) == Workshop.HighRisk)
    
    // 2. Перевірка середнього ризику (> 50.0 та <= 80.0) + граничне значення 80.0
    assert(Workshop.categorize(80.0) == Workshop.MediumRisk)
    assert(Workshop.categorize(60.0) == Workshop.MediumRisk)
    assert(Workshop.categorize(50.01) == Workshop.MediumRisk)

    // 3. Перевірка низького ризику (<= 50.0) + граничне значення 50.0
    assert(Workshop.categorize(50.0) == Workshop.LowRisk)
    assert(Workshop.categorize(10.0) == Workshop.LowRisk)
    assert(Workshop.categorize(0.0) == Workshop.LowRisk)


  test("Функція getMultiplier повинна повертати правильні коефіцієнти"):
    assert(Workshop.getMultiplier(Workshop.HighRisk) == 1.5)
    assert(Workshop.getMultiplier(Workshop.MediumRisk) == 1.2)
    assert(Workshop.getMultiplier(Workshop.LowRisk) == 1.0)