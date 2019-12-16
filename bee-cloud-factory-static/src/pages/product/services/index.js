import request from '@/utils/request'
import apis from './api'

export function getProductCategoryList(params) {
  return request(apis.getProductCategoryList.api(params), {
    method: apis.getProductCategoryList.type
  })
}
export function listConfigCodeByType(params) {
  return request(apis.listConfigCodeByType.api(params), {
    method: apis.listConfigCodeByType.type
  })
}
export function getProductById(params) {
  return request(apis.getProductById.api(params), {
    method: apis.getProductById.type
  })
}

export function searchProductList(str, params) {
  return request(apis.searchProductList.api(str), {
    method: apis.searchProductList.type,
    body: params
  })
}
export function saveProduct(params) {
  return request(apis.saveProduct.api(), {
    method: apis.saveProduct.type,
    body: params
  })
}
export function updateTestItem(params) {
  return request(apis.updateTestItem.api(), {
    method: apis.updateTestItem.type,
    body: params
  })
}
export function updateProductSpec(params) {
  return request(apis.updateProductSpec.api(), {
    method: apis.updateProductSpec.type,
    body: params
  })
}
export function getProductSpecByProductId(params) {
  return request(apis.getProductSpecByProductId.api(params), {
    method: apis.getProductSpecByProductId.type
  })
}
export function searchProductCategoryList(str, params) {
  return request(apis.searchProductCategoryList.api(str), {
    method: apis.searchProductCategoryList.type,
    body: params
  })
}
export function saveProductCategory(params) {
  return request(apis.saveProductCategory.api(), {
    method: apis.saveProductCategory.type,
    body: params
  })
}
export function deleteProductCategoryById(params) {
  return request(apis.deleteProductCategoryById.api(params), {
    method: apis.deleteProductCategoryById.type
  })
}
export function updateProductCategory(params) {
  return request(apis.updateProductCategory.api(), {
    method: apis.updateProductCategory.type,
    body: params
  })
}
export function updateProduct(params) {
  return request(apis.updateProduct.api(), {
    method: apis.updateProduct.type,
    body: params
  })
}

export function fileUpload(params) {
  return request(apis.fileUpload.api(), {
    method: apis.fileUpload.type,
    body: params,
    headers: {
      'Content-Type': 'multipart/form-data'
    }
  })
}


export function getTestAttributeByType(params) {
  return request(apis.getTestAttributeByType.api(), {
    method: apis.getTestAttributeByType.type,
    body: params
  })
}