import request from '@/utils/request';
import { api_user_prefix, api_factory_prefix } from '@/constants/prefix';

// 获取地磅顶部统计
export async function getWeightMachineWebCount(params) {
  return request(`${api_factory_prefix}/weightMachine/getWeightMachineWebCount?${params}`, {
    method: 'GET'
  });
}

// 获取用户供应商列表
export async function getCustomerListByCategory(params) {
  return request(`${api_user_prefix}/customer/getCustomerListByCategory`, {
    method: 'POST',
    body: {
      type: []
    }
  });
}

// 获取过磅车辆列表
export async function getWeightMachineWebList(str, params) {
  return request(`${api_factory_prefix}/weightMachine/getWeightMachineWebList?${str}`, {
    method: 'POST',
    body: params
  });
}

// 查询用户所属公司的承运商信息
export async function getCarrierInfoList() {
  return request(`${api_user_prefix}/supplier/getCarrierInfoList`, {
    method: `GET`
  });
}

// 根据车牌获取对应的承运商
export async function getLastCarrierByTrainNumber(str) {
  return request(`${api_factory_prefix}/weightMachine/getLastCarrierByTrainNumber?${str}`, {
    method: `GET`
  });
}

// 获取用户公司下所有客户和供应商
export async function getAllCustomerAndSupplier() {
  return request(`${api_user_prefix}/authCustomerOrSupplier/getAllCustomerAndSupplier`, {
    method: `GET`
  });
}

// 获取所有产品
export async function getProductListByCategory() {
  return request(`${api_factory_prefix}/configProduct/getProductListByCategory?category=0`, {
    method: `GET`
  });
}

// 获取地磅列表
export async function getWeightSelectList() {
  return request(`${api_factory_prefix}/configWeighDevice/getWeightSelectList?type=0`, {
    method: `GET`
  });
}

// 获取进厂重量
export async function getWeight(deviceId) {
  return request(`${api_factory_prefix}/weightMachine/getWeight?deviceId=${deviceId}`, {
    method: `GET`
  });
}

// 保存销售地磅数据信息
export async function saveSaleWeightMachine(params) {
  return request(`${api_factory_prefix}/weightMachine/saveSaleWeightMachine`, {
    method: `POST`,
    body: params
  });
}

// 根据磅单号获取详细信息
export async function getWeightMachineWebDeatil(str) {
  return request(`${api_factory_prefix}/weightMachine/getWeightMachineWebDeatil?${str}`, {
    method: `GET`
  });
}

// 确认进厂/出产称重
export async function confirmWeight(params) {
  return request(`${api_factory_prefix}/weightMachine/confirmWeight`, {
    method: `POST`,
    body: params
  });
}

// 确认扣重
export async function confirmDeductWeight(str) {
  return request(`${api_factory_prefix}/weightMachine/confirmDeductWeight?${str}`, {
    method: `GET`
  });
}

// 保存备注
export async function saveRemark(str) {
  return request(`${api_factory_prefix}/weightMachine/saveRemark?${str}`, {
    method: `GET`
  });
}

// 打印磅单
export async function printPoundSheet(params) {
  return request(`${api_factory_prefix}/print/printPoundSheet`, {
    method: `POST`,
    body: params
  });
}

// 继续运载
export async function continueMachine(params) {
  return request(`${api_factory_prefix}/weightMachine/continueMachine`, {
    method: `POST`,
    body: params
  });
}

// 结束运载
export async function confirmMachine(params) {
  return request(`${api_factory_prefix}/weightMachine/confirmMachine`, {
    method: `POST`,
    body: params
  });
}