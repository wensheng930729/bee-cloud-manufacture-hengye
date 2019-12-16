export const productData = (data) => {
  return [{
    productName: data.productName, qualityRequirement: data.qualityRequirement,
    unitPrice: data.unitPrice, quantity: data.quantity, amount: data.amount
  }]
}
