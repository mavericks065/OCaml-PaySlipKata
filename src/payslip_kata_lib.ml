open Base

type employee = {
  employee_id: String.t;
  employee_name: String.t;
  annual_gross_salary: Float.t;
}

module SalarySlip = struct
  type t = {
    employee_id: String.t;
    employee_name: String.t;
    monthly_gross_salary: Float.t;
    nic: Float.t;
  }
  [@@deriving compare]
end

module SalarySlipGenerator = struct
  let generate (_employee : employee) : SalarySlip.t  = 
    { 
      employee_id=_employee.employee_id; 
      employee_name=_employee.employee_name; 
      monthly_gross_salary= _employee.annual_gross_salary /. 12.0;
      nic = match Float.compare _employee.annual_gross_salary 8060.0 with 
      | z when (z > 0) -> (Float.sub _employee.annual_gross_salary 8060.0)*.0.01
      | _ -> 0.0
    }

  let%test "should_compute_monthly_gross_salary" = 
      Float.equal (generate 
                       { employee_id="1"; employee_name="r1"; annual_gross_salary=12000.0 ;}).monthly_gross_salary 1000.0

  let%test "should_compute_nic" = 
      Float.equal (generate 
                       { employee_id="1"; employee_name="r1"; annual_gross_salary=9060.0 ;}).nic 10.0
end