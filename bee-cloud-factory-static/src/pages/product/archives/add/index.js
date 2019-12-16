import { Component } from 'react'
import { Button, Card, Row, Col, Form, Input, Select, message, Table, Icon, Upload } from 'antd'
import styles from './index.less'
import withRouter from 'umi/withRouter'
import BreadCrumb from '../../../../components/BreadCrumb'
import {
  saveProduct,
  getProductById,
  updateProduct,
  getProductCategoryList,
  listConfigCodeByType,
  fileUpload
} from '../../services'
import router from 'umi/router'
import { api_factory_prefix,domain } from '@/constants/prefix';

const FormItem = Form.Item
const Option = Select.Option
const limitList = [
  {
    id: 0,
    value: '无限制'
  }, {
    id: 1,
    value: '整数'
  }, {
    id: 2,
    value: '数值（小数点后两位）'
  }, {
    id: 3,
    value: '百分比'
  }
]

@withRouter
@Form.create()
export default class Index extends Component {
  state = {
    data: [],
    getProductCategoryList: [],
    unitList: [],
    imageUrl: ''
  }

  id = this.props.location.query.id || null

  componentDidMount() {
    const { setFieldsValue } = this.props.form

    getProductCategoryList().then(res => {
      if (res.code === 1) {
        this.setState({
          getProductCategoryList: res.object
        })
      }
    })
    listConfigCodeByType('unit').then(res => {
      if (res.code === 1) {
        this.setState({
          unitList: res.object
        })
      }
    })
    if (this.id) {
      getProductById(this.id).then(res => {
        if (res.code === 1) {
          const data = res.object
          const obj = {
            categoryId: { key: data.categoryId, label: data.categoryName },
            unit: { key: data.unitCode, label: data.unitValue },
            name: data.name,
            status: data.status,
            standard: data.standard
          }
          this.setState({
            data: data.productSettlementAttributeDTOS,
            imageUrl: data.logo
          })
          setFieldsValue(obj)
        }
      })
    }
  }

  handleSubmit = () => {
    const { getFieldsValue } = this.props.form
    const { data, imageUrl } = this.state
    const values = getFieldsValue()
    const params = {
      categoryId: values.categoryId.key,
      categoryName: values.categoryId.label,
      unitCode: values.unit.key,
      unitValue: values.unit.label,
      status: values.status,
      logo: imageUrl,
      standard: values.standard,
      name: values.name
    }
    params.productSettlementAttributeSaveRQS = data
    console.log(params)
    if (this.id) {
      params.id = this.id
      updateProduct(params).then(res => {
        if (res.code === 1) {
          message.success(res.message)
          router.goBack()
        } else {
          message.error(res.message)
        }
      })
    } else {
      saveProduct(params).then(res => {
        if (res.code === 1) {
          message.success(res.message)
          router.goBack()
        } else {
          message.error(res.message)
        }
      })
    }
  }

  handleAdd = () => {
    const { data } = this.state
    // const id = data.length + 1
    const obj = {
      settlementItem: '',
      fieldLimit: 0,
      status: 1,
      unit: ''
    }
    data.push(obj)
    this.setState({
      data
    })
  }

  handleDelete = index => {
    const { data } = this.state
    data.splice(index, 1)
    this.setState({
      data
    })
  }

  handleChange = (e, name, dataIndex) => {
    const { data } = this.state
    data[dataIndex][name] = e
    this.setState({
      data
    })
  }

  handleUploadChange = info => {
    if (info.file.status === 'done') {
      const res = info.file.response
      if (res.code === 1) {
        this.setState({
          imageUrl: res.object.access_url
        })
      } else {
        message.error(res.message)
      }
    }
  }

  render() {
    const { data, getProductCategoryList, unitList, imageUrl } = this.state
    const { getFieldDecorator } = this.props.form
    const self = this
    const uploadButton = (
      <div>
        <Icon type={this.state.loading ? 'loading' : 'plus'} />
        <div className="ant-upload-text">Upload</div>
      </div>
    )
    const columns = [
      {
        title: '产品结算项',
        dataIndex: 'settlementItem',
        width: '20%',
        render: (h, row, index) => <Input placeholder="产品结算项" value={h} onChange={e => self.handleChange(e.target.value, 'settlementItem', index)} />
      }, {
        title: '字段限制',
        dataIndex: 'fieldLimit',
        width: '20%',
        render: (h, row, index) => (
          <Select value={h} onChange={e => self.handleChange(e, 'fieldLimit', index)} placeholder="字段限制" style={{ width: '100%' }}>
            {limitList.map(item => {
              return (
                <Option value={item.id} key={item.id}>
                  {item.value}
                </Option>
              )
            })}
          </Select>
        )
      }, {
        title: '单位',
        dataIndex: 'unit',
        width: '20%',
        render: (h, row, index) => <Input placeholder="单位" value={h} onChange={e => self.handleChange(e.target.value, 'unit', index)} />
      }, {
        title: '状态',
        dataIndex: 'status',
        width: '20%',
        render: (h, row, index) => (
          <Select value={h} onChange={e => self.handleChange(e, 'status', index)} placeholder="状态" style={{ width: '100%' }}>
            <Option value={1}>启用</Option>
            <Option value={0}>停用</Option>
          </Select>
        )
      }, {
        title: '操作',
        dataIndex: 'action',
        render: (h, row, index) => <span style={{ color: '#1890ff', cursor: 'pointer' }} onClick={() => self.handleDelete(index)}>删除</span>
      }
    ]
    return (
      <div>
        <BreadCrumb extra={<Button type="primary" onClick={() => router.goBack()}>返回</Button>} />
        <div className={styles.container}>
          <Card title="基本信息" bordered={false} extra={
            <div className={styles.btnBox}>
              <Button type="primary" onClick={this.handleSubmit}>
                {this.id ? '修改' : '保存'}
              </Button>
              <Button onClick={() => router.goBack()}>取消</Button>
            </div>
          }>
            <Form>
              <Row style={{ marginLeft: '20%' }}>
                <Col span={12}>
                  <FormItem label="产品名称">
                    {getFieldDecorator('name', {
                      rules: [{ required: true, message: '请填入设备名称' }]
                    })(
                      <Input style={{ width: 260 }} placeholder="请输入" />
                    )}
                  </FormItem>
                </Col>
                <Col span={12}>
                  <FormItem label="产品类别">
                    {getFieldDecorator('categoryId', {
                      rules: [{ required: true, message: '请选择产品类别' }]
                    })(
                      <Select style={{ width: 260 }} placeholder="请选择" labelInValue>
                        {getProductCategoryList.map((item, index) => {
                          return (
                            <Option value={item.id} key={item.id}>
                              {item.name}
                            </Option>
                          )
                        })}
                      </Select>
                    )}
                  </FormItem>
                </Col>
                <Col span={12}>
                  <FormItem label="计量单位">
                    {getFieldDecorator('unit', {
                      rules: [{ required: true, message: '请选择计量单位' }]
                    })(
                      <Select style={{ width: 260 }} placeholder="请选择" labelInValue>
                        {unitList.map((item, index) => {
                          return (
                            <Option value={item.code} key={item.code}>
                              {item.value}
                            </Option>
                          )
                        })}
                      </Select>
                    )}
                  </FormItem>
                </Col>
                <Col span={12}>
                  <FormItem label="状态">
                    {getFieldDecorator('status', {
                      rules: [{ required: true, message: '请选择设备状态' }]
                    })(
                      <Select style={{ width: 260 }} placeholder="请选择">
                        <Option value={1}>启用</Option>
                        <Option value={0}>停用</Option>
                      </Select>
                    )}
                  </FormItem>
                </Col>
                <Col span={12}>
                  <FormItem label="是否是标准品">
                    {getFieldDecorator('standard', {
                      rules: [{ required: true, message: '请选择是否是标准品' }]
                    })(
                      <Select style={{ width: 260 }} placeholder="请选择">
                        <Option value={1}>是</Option>
                        <Option value={0}>否</Option>
                      </Select>
                    )}
                  </FormItem>
                </Col>
                <Col span={12}>
                  <FormItem label="上传图片">
                    {getFieldDecorator('logo')(
                      <Upload
                        name="file"
                        listType="picture-card"
                        className="avatar-uploader"
                        showUploadList={false}
                        action={domain +api_factory_prefix+ '/file/upload?type=2'}
                        onChange={this.handleUploadChange}
                      // onPreview={this.handlePreview}
                      >
                        {
                          imageUrl ? (
                            <img src={imageUrl} alt="avatar" style={{ width: '100%' }} />
                          ) : (
                              uploadButton
                            )
                        }
                      </Upload>
                    )}
                  </FormItem>
                </Col>
              </Row>
            </Form>
          </Card>
          <Card title="结算属性" bordered={false}>
            <Table
              rowKey="id"
              columns={columns}
              dataSource={data}
              pagination={false}
            />
            <div className={styles.btn_add} onClick={this.handleAdd}>添加一行</div>
          </Card>
        </div>
      </div>
    )
  }
}
