export interface Product {
    id: number;
    name: string;
    price: number;
    description?: string;
}
    
export const products = [
    {
	id: 0,
	name: "Phone",
	price: 10,
	description: "A smart phone"
    },
    {
	id: 1,
	name: "keyboard",
	price: 8
    }
];
    
    
