(module websubmit-strings mzscheme
  
  (provide web-submission-login-title
           partner-login-title
           incorrect-login-title
           select-assignment-title
           upload-homework-title
           upload-confirm-title
           upload-complete-title
           sorry-charlie-title
           change-pass-title
           manage-account-title
           partner-already-paired-title
           need-partner-title
           
           select-grader-title
           select-grader-custom-title
           
           create-account-title
           
           choose-credentials-title
           choose-credentials-custom-title
           
           add-or-proceed-title
           add-or-proceed-custom-title
           
           account-summary-title
           account-summary-custom-title
           
           account-exists-title
           
           initial-login-message
           invalid-credentials-message
           
           initial-partner-login-message
           partner-invalid-credentials-message
           
           select-grader-instructions
           current-grader-info
           no-current-grader-info
           
           initial-choose-credentials-message
           
           initial-changepass-message
           password-mismatch-message

	   initial-upload-message
           missing-both-uploads-message
           missing-def-upload-message
           missing-int-upload-message
           
           cant-find-id-message
           initial-create-account-message
           
           
           ;; **************************************************
           ;; staff pages:
           
           staff-lobby-title
           staff-change-pass-title
           
           create-new-student-title
           initial-create-student-message
           student-id-exists-message
           
           review-new-student-title
           
           get-student-to-edit-title
           initial-student-to-edit-message
           cant-find-students-id-message
           
           edit-student-title
           initial-edit-student-message
           review-edit-student-title
           login-name-exists-message
           
           )
   
   
   
   ;; web-page-titles
  (define web-submission-login-title "Web Submission Login")
  (define partner-login-title "New Partner Login")
  (define incorrect-login-title "Incorrect Login")
  (define select-assignment-title "Select Assignment")
  (define upload-homework-title "Upload Homework File")
  (define upload-confirm-title "Upload Confirm")
  (define upload-complete-title "Upload Complete")
  (define sorry-charlie-title "Sorry Charlie")
  (define change-pass-title "Change Password")
  (define manage-account-title "Manage Account")
  (define partner-already-paired-title "Student Already Has Partners")
  (define need-partner-title "You Need a Partner")
  
  (define select-grader-title "Select a TA")
  (define select-grader-custom-title "Select a TA for ~a")
  
  (define create-account-title "Create Account")
  
  (define choose-credentials-title "Choose Username and Password")
  (define choose-credentials-custom-title "Choose Username and Password for ~a")
  
  (define add-or-proceed-title "Add Partner or Proceed to Account Summary")
  (define add-or-proceed-custom-title "~a: Add Partner or Proceed to Account Summary")
  
  (define account-summary-title "Account Summary")
  (define account-summary-custom-title "Account Summary for ~a")
  
  (define account-exists-title "Your Account Already Exists")
  
  (define select-grader-instructions
    (string-append "You must specify who your TA is before you can submit homework. "
                   "If you change your TA, this will change the grader for your homework partner(s) too."))
  (define current-grader-info "Currently, your TA is ~a")
  (define no-current-grader-info "Currently, you have no TA")
  
  
  (define initial-changepass-message "Please enter your new password. (make sure caps-lock is off.)")
  (define password-mismatch-message "Your password entries didn't match, please reenter your new password")
  
  (define initial-upload-message "Please provide the files to submit for assignment: ")
  (define missing-both-uploads-message "You are missing both definitions and test cases uploads for assignment: ")
  (define missing-def-upload-message "You are missing a definitions upload for assignment: ")
  (define missing-int-upload-message "You are missing a test cases upload for assignment: ")
  (define cant-find-id-message "Not able to find to find your id. Please try again.")
  (define initial-create-account-message
    (string-append "The next series of pages will guide you through the"
                   " process of creating an account for homework submission"))
  (define initial-choose-credentials-message
    "Please choose a login name and password for your account")
  
  
  
  
  
  (define initial-login-message "Please enter your login name and password")
  (define invalid-credentials-message "Your login information was incorrect. Please reenter your login name and password")
  
  (define initial-partner-login-message
    "Please enter the login name and password for the student who will join ~a")
  
  (define partner-invalid-credentials-message
    "Your login information was incorrect. Please reenter the login name and password for the student who will join ~a")
  
  (define staff-lobby-title "Staff Options")
  (define staff-change-pass-title "Staff Change Password")
  (define create-new-student-title "Create New Student Account")
  (define initial-create-student-message "Enter the 9-digit ID and name for the new student.")
  (define review-new-student-title "Review New Student")
  
  (define get-student-to-edit-title "Specify Student Account")
  (define initial-student-to-edit-message "Please enter the 9-digit ID for the account you want to edit")
  
  (define edit-student-title "Edit Student Account")
  (define review-edit-student-title "Review Student Account Changes")
  (define initial-edit-student-message "Specify changes for the following student")
  (define student-id-exists-message "The student id you entered already exists")
  (define cant-find-students-id-message "Could not find the student's id. Please try again.")
  (define login-name-exists-message "The new login name you have chosen for the student already exists.")
  )
    
    
    
  
      
      
