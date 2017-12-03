function recaptcha(input) {
  let sum = 0;

  input = input + input[0]
  for (let i = 1;i<input.length;i++) {
    if (input[i] === input[i-1]) {
      sum += parseInt(input[i])
    }
  }

  return sum;
}

function recaptcha2(input) {
  let sum = 0;

  for (let i = 0;i<input.length;i++) {
    const otherIndex = (i + (input.length / 2)) % input.length
    console.log('comparing', i, otherIndex)
    if (input[i] === input[otherIndex]) {
      sum += parseInt(input[i])
    }
  }

  return sum;
}
