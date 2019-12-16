import request from '@/utils/request';
import apis from './api';

export function getFiles(params) {
  return request(apis.warehouseFile.getFiles.api(params), {
    method: apis.warehouseFile.getFiles.type
  })
}

export function updateFiles(params) {
  return request(apis.warehouseFile.updateFiles.api(), {
    method: apis.warehouseFile.updateFiles.type,
    body: params
  })
}

export function addFiles(params) {
  return request(apis.warehouseFile.addFiles.api(), {
    method: apis.warehouseFile.addFiles.type,
    body: params
  })
}

export function getStocks({ currentPage, pageSize, params }) {
  return request(apis.stock.getStocks.api({ currentPage, pageSize }), {
    method: apis.stock.getStocks.type,
    body: params
  })
}

export function getStockDetail(id) {
  return request(apis.stock.getStockDetail.api(id), {
    method: apis.stock.getStockDetail.type
  })
}

export function getOrderNumber() {
  return request(apis.stock.getOrderNumber.api(), {
    method: apis.stock.getOrderNumber.type
  })
}

export function getProducts() {
  return request(apis.stock.getProducts.api(), {
    method: apis.stock.getProducts.type
  })
}

export function getProductSpec(id) {
  return request(apis.stock.getProductSpec.api(id), {
    method: apis.stock.getProductSpec.type
  })
}

export function getWarehouse() {
  return request(apis.stock.getWarehouse.api(), {
    method: apis.stock.getWarehouse.type,
    body: { type: [] }
  })
}

export function saveStock(params) {
  return request(apis.stock.saveStock.api(), {
    method: apis.stock.saveStock.type,
    body: params
  })
}