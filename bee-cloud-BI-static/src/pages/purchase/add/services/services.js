import request from '@/utils/request'
import { api_user_prefix, api_factory_prefix } from '@/constants/prefix';

// 获取所有供应商
export async function getSupplierListByCategory() {
  return request(`${api_user_prefix}/supplier/getSupplierListByCategory`, {
    method: "POST",
    body: {
      type: []
    }
  });
}

// 获取所有产品
export async function getProducts() {
  return request(`${api_factory_prefix}/buyContractBasic/getProducts`, {
    method: "GET"
  });
}

// 获取所有地点
export async function getLocationList() {
  return request(`${api_factory_prefix}/configLocation/getLocationList`, {
    method: "GET"
  });
}



// 新增采购合同
export async function addContractBuy(params) {
  return request(`${api_factory_prefix}/buyContractBasic/addContractBuy`, {
    method: "POST",
    body: params
  });
}